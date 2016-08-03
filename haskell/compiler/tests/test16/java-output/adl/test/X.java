package adl.test;

import org.adl.runtime.Factory;

public class X {

  /* Members */

  private adl.test2.A value;

  /* Constructors */

  public X(adl.test2.A value) {
    this.value = java.util.Objects.requireNonNull(value);
  }

  public X() {
    this.value = new adl.test2.A();
  }

  public X(X other) {
    this.value = adl.test2.A.FACTORY.create(other.value);
  }

  /* Accessors and mutators */

  public adl.test2.A getValue() {
    return value;
  }

  public void setValue(adl.test2.A newValue) {
    value = java.util.Objects.requireNonNull(newValue);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof X)) {
      return false;
    }
    X other = (X) other0;
    return
      value.equals(other.value);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + value.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static final Factory<X> FACTORY = new Factory<X>() {
    public X create() {
      return new X();
    }
    public X create(X other) {
      return new X(other);
    }
  };
}
