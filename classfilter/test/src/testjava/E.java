package testjava;

import testjava.sub.leaf.OtherException;

public class E {
    void t() throws MyException {}
    void tt() {
        throw new OtherException("sub");
    }
}
