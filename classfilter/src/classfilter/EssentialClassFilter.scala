package classfilter

import org.apache.bcel.Repository
import org.apache.bcel.classfile.*
import org.apache.bcel.generic.{ArrayType, ObjectType, Type}

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}
import java.util.regex.Pattern
import scala.compiletime.uninitialized
import scala.util.Try
import scala.util.control.NonFatal
import scala.jdk.CollectionConverters.*

/**
 * Loosely based on https://github.com/apache/commons-bcel/blob/master/src/examples/TransitiveHull.java
 */
object EssentialClassFilter {

  // since these classes are always provided by java, we do not want to list these
  // classes in the the result:
  private val JavaStandardIgnores: IndexedSeq[String] = scala.collection.immutable.ArraySeq(
    "java[.].*", "javax[.].*", "sun[.].*", "sunw[.].*", "com[.]sun[.].*", "org[.]omg[.].*", "org[.]w3c[.].*", "org[.]xml[.].*", "net[.]jini[.].*"
  )

  private val InitialBufferSize: Int = 2096

  /**
   * Returns all transitively dependent classes for the given input classes
   * @param inputClasses fully qualified class names
   * @return transitive closure of classes
   */
  def getClassNames(inputClasses: String*): Array[String] = {
    try {
      val myInputClasses: Array[JavaClass] = inputClasses.map(cn =>
        Try(Repository.lookupClass(cn)).getOrElse(new ClassParser(cn).parse)
      ).toArray
      val hull = new ClassHull(myInputClasses, JavaStandardIgnores, InitialBufferSize, transitive = true)
      hull.start()
      hull.getClassNames.toArray.sorted // note: sort is not really necessary
    } catch {
      case e: Exception =>
        throw new RuntimeException("Could not detect depending class names", e)
    }
  }

  /**
   * Returns a dependency graph mapping each class to its dependencies
   * @param inputClasses fully qualified class names
   * @return transitive closure of classes, add to each class name its dependencies
   */
  def getDependencyGraph(inputClasses: String*): ConcurrentHashMap[String, Array[String]] = {
    val allDependClasses = getClassNames(inputClasses *)
    val dependencyRelations = new ConcurrentHashMap[String, Array[String]](allDependClasses.length)
    for (curClass <- allDependClasses) {
      val curClassAsJavaClass = Array(Try(Repository.lookupClass(curClass)).getOrElse(new ClassParser(curClass).parse))
      val hull = new ClassHull(curClassAsJavaClass, JavaStandardIgnores, InitialBufferSize, transitive = false)
      hull.start()
      val curDependencies = hull.getClassNames.filterNot(_ == curClass).toArray.sorted
      dependencyRelations.put(curClass, curDependencies)
    }
    dependencyRelations
  }

  private class ClassHull(
    clazzes: Array[JavaClass],
    private val ignoredClassPatterns: IndexedSeq[String],
    initialBufferSize: Int,
    private val transitive: Boolean
  ) extends org.apache.bcel.classfile.EmptyVisitor {
    private val curIgnoredClassNames: ConcurrentHashMap.KeySetView[String, java.lang.Boolean] = ConcurrentHashMap.newKeySet[String](initialBufferSize)
    private val queue: ConcurrentLinkedQueue[JavaClass] = new ConcurrentLinkedQueue[JavaClass]()
    private val set: ConcurrentHashMap[String, JavaClass] = new ConcurrentHashMap[String, JavaClass](initialBufferSize)
    private var cp:ConstantPool = uninitialized

    init(clazzes)

    private def init(clazzes: Array[JavaClass]): Unit = {
      fastFor(clazzes)(clazz => {
        queue.add(clazz)
        set.putIfAbsent(clazz.getClassName, clazz)
      })
    }

    /**
     * Start traversal using DescendingVisitor pattern.
     */
    def start(): Unit = {
      while (!queue.isEmpty) {
        val clazz = queue.poll()
        cp = clazz.getConstantPool
        new DescendingVisitor(clazz, this).visit()
      }
    }

    private def addClassByName(className: String): Unit = {
      val myClassName = Utility.pathToPackage(className)
      val isIgnored   = set.containsKey(myClassName) || curIgnoredClassNames.contains(className) || ignoredClassPatterns.exists(p => Pattern.matches(p, myClassName))
      if (!isIgnored) {
        try {
          val clazz = Repository.lookupClass(className)
          if (set.putIfAbsent(clazz.getClassName, clazz) != null && transitive) {
            queue.add(clazz)
          }
          fastFor(clazz.getFields) {
            f => visitField(f)
          }
          fastFor(clazz.getMethods) {
            m => visitMethod(m)
          }
        } catch {
          case e: ClassNotFoundException =>
            curIgnoredClassNames.add(myClassName)
        }
      }
    }

    def getClassNames: Set[String] = {
      set.keys().asScala.toSet
    }

    override def visitConstantClass(cc: ConstantClass): Unit = {
      val className = cc.getConstantValue(cp).asInstanceOf[String]
      addClassByName(className)
    }

    override def visitField(f: Field): Unit = {
      val signature = f.getSignature
      checkType(Type.getType(signature))
      fastFor(f.getAnnotationEntries) {
        a => checkType(Type.getType(a.getAnnotationType))
      }
    }

    override def visitMethod(m: Method): Unit = {
      val signature = m.getSignature
      val curType = Type.getReturnType(signature)
      checkType(curType)
      // find types linked by signature
      fastFor(Type.getArgumentTypes(signature)) {
        curArgType => checkType(curArgType)
      }
      // find types linked by local variables
      val lvt = m.getLocalVariableTable
      if (lvt != null) {
        visitLocalVariableTable(lvt)
      }
      // find types linked by declared exceptions:
      val ext = m.getExceptionTable
      if(ext != null){
        visitExceptionTable(ext)
      }
      // find types linked by as thrown runtime exceptions:
      // note: without the Java standard exceptions, like ArithmeticException (zero division) etc.:
      val cpt = m.getConstantPool
      if (cpt != null) {
        fastFor (cpt.getConstantPool) {
          case c: ConstantClass =>
            cpt.getConstantPool()(c.getNameIndex) match {
              case u8: ConstantUtf8 =>
                val testClassName = u8.getBytes
                val realClassName = Utility.pathToPackage(testClassName)
                try {
                  addClassByName(realClassName)
                } catch {
                  case NonFatal(ex) => // ignore
                    curIgnoredClassNames.add(realClassName)
                }
              case _ =>
            }
          case _ =>
        }
        for(a <- m.getAnnotationEntries){
          checkType(Type.getType(a.getAnnotationType))
        }
      }
    }

    override def visitLocalVariableTable(lvt: LocalVariableTable): Unit = {
      val lv: Array[LocalVariable] = lvt.getLocalVariableTable
      fastFor(lv)(l => {
        val localSignature = l.getSignature
        checkType(Type.getType(localSignature))
      })
    }

    override def visitExceptionTable(ext: ExceptionTable): Unit = {
      val ex: Array[String] = ext.getExceptionNames
      fastFor(ex) {
        e => addClassByName(e)
      }
    }

    private def checkType(curType: Type): Unit = {
      var myType = curType
      myType match {
        case arrayType: ArrayType =>
          myType = arrayType.getBasicType
        case _ =>
      }
      myType match {
        case objectType: ObjectType =>
          addClassByName(objectType.getClassName)
        case _ =>
      }
    }

    override def visitAnnotation(anno: Annotations): Unit = {
      fastFor(anno.getAnnotationEntries) {
        a => checkType(Type.getType(a.getAnnotationType))
      }
    }

    inline private def fastFor[T](data: Array[T])(apply: T=> Unit): Unit = {
      val l = data.length
      var i = 0
      while(i < l){
        apply(data(i))
        i+=1
      }
    }
  }

}
