package adl.test;

import org.adl.runtime.Factory;

public class S {

  /* Members */

  private java.time.LocalDate v1;
  private java.time.LocalDate v2;
  private java.time.LocalDate v3;
  private java.time.LocalDate v4;

  /* Constructors */

  public S(java.time.LocalDate v1, java.time.LocalDate v2, java.time.LocalDate v3, java.time.LocalDate v4) {
    this.v1 = java.util.Objects.requireNonNull(v1);
    this.v2 = java.util.Objects.requireNonNull(v2);
    this.v3 = java.util.Objects.requireNonNull(v3);
    this.v4 = java.util.Objects.requireNonNull(v4);
  }

  public S() {
    this.v1 = helpers.DateHelpers.FACTORY.create();
    this.v2 = helpers.DateHelpers.create("2000-01-01");
    this.v3 = helpers.CDateHelpers.FACTORY.create();
    this.v4 = helpers.CDateHelpers.create((short)2000, (short)1, (short)1);
  }

  public S(S other) {
    this.v1 = helpers.DateHelpers.FACTORY.create(other.v1);
    this.v2 = helpers.DateHelpers.FACTORY.create(other.v2);
    this.v3 = helpers.CDateHelpers.FACTORY.create(other.v3);
    this.v4 = helpers.CDateHelpers.FACTORY.create(other.v4);
  }

  /* Accessors and mutators */

  public java.time.LocalDate getV1() {
    return v1;
  }

  public void setV1(java.time.LocalDate newV1) {
    v1 = java.util.Objects.requireNonNull(newV1);
  }

  public java.time.LocalDate getV2() {
    return v2;
  }

  public void setV2(java.time.LocalDate newV2) {
    v2 = java.util.Objects.requireNonNull(newV2);
  }

  public java.time.LocalDate getV3() {
    return v3;
  }

  public void setV3(java.time.LocalDate newV3) {
    v3 = java.util.Objects.requireNonNull(newV3);
  }

  public java.time.LocalDate getV4() {
    return v4;
  }

  public void setV4(java.time.LocalDate newV4) {
    v4 = java.util.Objects.requireNonNull(newV4);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof S)) {
      return false;
    }
    S other = (S) other0;
    return
      v1.equals(other.v1) &&
      v2.equals(other.v2) &&
      v3.equals(other.v3) &&
      v4.equals(other.v4);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + v1.hashCode();
    result = result * 37 + v2.hashCode();
    result = result * 37 + v3.hashCode();
    result = result * 37 + v4.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static final Factory<S> FACTORY = new Factory<S>() {
    public S create() {
      return new S();
    }
    public S create(S other) {
      return new S(other);
    }
  };
}
