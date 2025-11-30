package testjava;

import testjava.sub.CAnnotation;
import testjava.sub.FAnnotation;
import testjava.sub.MAnnotation;
import testjava.sub.leaf.AAA;

@CAnnotation
public class A extends B implements I {
    C test;
    @FAnnotation
    MyEnum e;

    class Inner {
        D getD() {
            return new D();
        }
        E e;

        @MAnnotation
        int calc() {
            G g = new G();
            g.s(() -> new SI());
            return g.t();
        }
    }

    C getC(F f) {
        return new C();
    }

    int calc() {
        H h = new H();
        return h.t();
    }

    void otherPackage() {
        AAA a = null;
    }

    static class OtherInner {
        static J m(K s) { return new J();}
    }
}
