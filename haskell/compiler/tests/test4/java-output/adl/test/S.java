package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class S {

  /* Members */

  private java.time.LocalDate v1;
  private java.time.LocalDate v2;
  private java.time.LocalDate v3;
  private java.time.LocalDate v4;
  private java.util.Optional<String> v5;
  private sys.types.Pair<String, Integer> v6;
  private java.util.HashSet<Integer> v7;
  private java.util.HashMap<String, Integer> v8;

  /* Constructors */

  public S(java.time.LocalDate v1, java.time.LocalDate v2, java.time.LocalDate v3, java.time.LocalDate v4, java.util.Optional<String> v5, sys.types.Pair<String, Integer> v6, java.util.HashSet<Integer> v7, java.util.HashMap<String, Integer> v8) {
    this.v1 = java.util.Objects.requireNonNull(v1);
    this.v2 = java.util.Objects.requireNonNull(v2);
    this.v3 = java.util.Objects.requireNonNull(v3);
    this.v4 = java.util.Objects.requireNonNull(v4);
    this.v5 = java.util.Objects.requireNonNull(v5);
    this.v6 = java.util.Objects.requireNonNull(v6);
    this.v7 = java.util.Objects.requireNonNull(v7);
    this.v8 = java.util.Objects.requireNonNull(v8);
  }

  public S() {
    this.v1 = helpers.DateHelpers.FACTORY.create();
    this.v2 = helpers.DateHelpers.create("2000-01-01");
    this.v3 = helpers.CDateHelpers.FACTORY.create();
    this.v4 = helpers.CDateHelpers.create((short)2000, (short)1, (short)1);
    this.v5 = adl.runtime.OptionalHelpers.factory(Factories.STRING).create();
    this.v6 = sys.types.Pair.factory(Factories.STRING, Factories.INTEGER).create();
    this.v7 = adl.runtime.HashSetHelpers.create(java.util.Arrays.asList(1, 2, 3));
    this.v8 = adl.runtime.HashMapHelpers.factory(Factories.STRING, Factories.INTEGER).create();
  }

  public S(S other) {
    this.v1 = helpers.DateHelpers.FACTORY.create(other.v1);
    this.v2 = helpers.DateHelpers.FACTORY.create(other.v2);
    this.v3 = helpers.CDateHelpers.FACTORY.create(other.v3);
    this.v4 = helpers.CDateHelpers.FACTORY.create(other.v4);
    this.v5 = adl.runtime.OptionalHelpers.factory(Factories.STRING).create(other.v5);
    this.v6 = sys.types.Pair.factory(Factories.STRING, Factories.INTEGER).create(other.v6);
    this.v7 = adl.runtime.HashSetHelpers.factory(Factories.INTEGER).create(other.v7);
    this.v8 = adl.runtime.HashMapHelpers.factory(Factories.STRING, Factories.INTEGER).create(other.v8);
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

  public java.util.Optional<String> getV5() {
    return v5;
  }

  public void setV5(java.util.Optional<String> newV5) {
    v5 = java.util.Objects.requireNonNull(newV5);
  }

  public sys.types.Pair<String, Integer> getV6() {
    return v6;
  }

  public void setV6(sys.types.Pair<String, Integer> newV6) {
    v6 = java.util.Objects.requireNonNull(newV6);
  }

  public java.util.HashSet<Integer> getV7() {
    return v7;
  }

  public void setV7(java.util.HashSet<Integer> newV7) {
    v7 = java.util.Objects.requireNonNull(newV7);
  }

  public java.util.HashMap<String, Integer> getV8() {
    return v8;
  }

  public void setV8(java.util.HashMap<String, Integer> newV8) {
    v8 = java.util.Objects.requireNonNull(newV8);
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
      v4.equals(other.v4) &&
      v5.equals(other.v5) &&
      v6.equals(other.v6) &&
      v7.equals(other.v7) &&
      v8.equals(other.v8);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + v1.hashCode();
    result = result * 37 + v2.hashCode();
    result = result * 37 + v3.hashCode();
    result = result * 37 + v4.hashCode();
    result = result * 37 + v5.hashCode();
    result = result * 37 + v6.hashCode();
    result = result * 37 + v7.hashCode();
    result = result * 37 + v8.hashCode();
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
