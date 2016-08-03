package adl.test2;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class A {

  /* Members */

  private int a;

  /* Constructors */

  public A(int a) {
    this.a = a;
  }

  public A() {
    this.a = 0;
  }

  public A(A other) {
    this.a = other.a;
  }

  /* Accessors and mutators */

  public int getA() {
    return a;
  }

  public void setA(int newA) {
    a = newA;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof A)) {
      return false;
    }
    A other = (A) other0;
    return
      a == other.a;
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + a;
    return result;
  }

  /* Factory for construction of generic values */

  public static final Factory<A> FACTORY = new Factory<A>() {
    public A create() {
      return new A();
    }
    public A create(A other) {
      return new A(other);
    }
  };
}
