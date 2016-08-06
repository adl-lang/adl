package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import java.util.Arrays;
import java.util.Objects;

public class S {

  /* Members */

  private java.time.LocalDate v1;
  private java.time.LocalDate v2;
  private java.time.LocalDate v3;
  private java.time.LocalDate v4;
  private java.util.Optional<String> v5;
  private java.util.Optional<String> v5a;
  private java.util.Optional<String> v5b;
  private adl.sys.types.Pair<String, Integer> v6;
  private java.util.HashSet<Integer> v7;
  private java.util.HashSet<Integer> v7a;
  private java.util.HashMap<String, Integer> v8;

  /* Constructors */

  public S(java.time.LocalDate v1, java.time.LocalDate v2, java.time.LocalDate v3, java.time.LocalDate v4, java.util.Optional<String> v5, java.util.Optional<String> v5a, java.util.Optional<String> v5b, adl.sys.types.Pair<String, Integer> v6, java.util.HashSet<Integer> v7, java.util.HashSet<Integer> v7a, java.util.HashMap<String, Integer> v8) {
    this.v1 = Objects.requireNonNull(v1);
    this.v2 = Objects.requireNonNull(v2);
    this.v3 = Objects.requireNonNull(v3);
    this.v4 = Objects.requireNonNull(v4);
    this.v5 = Objects.requireNonNull(v5);
    this.v5a = Objects.requireNonNull(v5a);
    this.v5b = Objects.requireNonNull(v5b);
    this.v6 = Objects.requireNonNull(v6);
    this.v7 = Objects.requireNonNull(v7);
    this.v7a = Objects.requireNonNull(v7a);
    this.v8 = Objects.requireNonNull(v8);
  }

  public S() {
    this.v1 = helpers.DateHelpers.FACTORY.create();
    this.v2 = helpers.DateHelpers.create("2000-01-01");
    this.v3 = helpers.CDateHelpers.FACTORY.create();
    this.v4 = helpers.CDateHelpers.create((short)2000, (short)1, (short)1);
    this.v5 = org.adl.runtime.OptionalHelpers.factory(Factories.STRING).create();
    this.v5a = org.adl.runtime.OptionalHelpers.nothing(null);
    this.v5b = org.adl.runtime.OptionalHelpers.just("hello");
    this.v6 = adl.sys.types.Pair.factory(Factories.STRING, Factories.INTEGER).create();
    this.v7 = org.adl.runtime.HashSetHelpers.create(Arrays.asList(1, 2, 3));
    this.v7a = org.adl.runtime.HashSetHelpers.factory(Factories.INTEGER).create();
    this.v8 = org.adl.runtime.HashMapHelpers.factory(Factories.STRING, Factories.INTEGER).create();
  }

  public S(S other) {
    this.v1 = helpers.DateHelpers.FACTORY.create(other.v1);
    this.v2 = helpers.DateHelpers.FACTORY.create(other.v2);
    this.v3 = helpers.CDateHelpers.FACTORY.create(other.v3);
    this.v4 = helpers.CDateHelpers.FACTORY.create(other.v4);
    this.v5 = org.adl.runtime.OptionalHelpers.factory(Factories.STRING).create(other.v5);
    this.v5a = org.adl.runtime.OptionalHelpers.factory(Factories.STRING).create(other.v5a);
    this.v5b = org.adl.runtime.OptionalHelpers.factory(Factories.STRING).create(other.v5b);
    this.v6 = adl.sys.types.Pair.factory(Factories.STRING, Factories.INTEGER).create(other.v6);
    this.v7 = org.adl.runtime.HashSetHelpers.factory(Factories.INTEGER).create(other.v7);
    this.v7a = org.adl.runtime.HashSetHelpers.factory(Factories.INTEGER).create(other.v7a);
    this.v8 = org.adl.runtime.HashMapHelpers.factory(Factories.STRING, Factories.INTEGER).create(other.v8);
  }

  /* Accessors and mutators */

  public java.time.LocalDate getV1() {
    return v1;
  }

  public void setV1(java.time.LocalDate newV1) {
    v1 = Objects.requireNonNull(newV1);
  }

  public java.time.LocalDate getV2() {
    return v2;
  }

  public void setV2(java.time.LocalDate newV2) {
    v2 = Objects.requireNonNull(newV2);
  }

  public java.time.LocalDate getV3() {
    return v3;
  }

  public void setV3(java.time.LocalDate newV3) {
    v3 = Objects.requireNonNull(newV3);
  }

  public java.time.LocalDate getV4() {
    return v4;
  }

  public void setV4(java.time.LocalDate newV4) {
    v4 = Objects.requireNonNull(newV4);
  }

  public java.util.Optional<String> getV5() {
    return v5;
  }

  public void setV5(java.util.Optional<String> newV5) {
    v5 = Objects.requireNonNull(newV5);
  }

  public java.util.Optional<String> getV5a() {
    return v5a;
  }

  public void setV5a(java.util.Optional<String> newV5a) {
    v5a = Objects.requireNonNull(newV5a);
  }

  public java.util.Optional<String> getV5b() {
    return v5b;
  }

  public void setV5b(java.util.Optional<String> newV5b) {
    v5b = Objects.requireNonNull(newV5b);
  }

  public adl.sys.types.Pair<String, Integer> getV6() {
    return v6;
  }

  public void setV6(adl.sys.types.Pair<String, Integer> newV6) {
    v6 = Objects.requireNonNull(newV6);
  }

  public java.util.HashSet<Integer> getV7() {
    return v7;
  }

  public void setV7(java.util.HashSet<Integer> newV7) {
    v7 = Objects.requireNonNull(newV7);
  }

  public java.util.HashSet<Integer> getV7a() {
    return v7a;
  }

  public void setV7a(java.util.HashSet<Integer> newV7a) {
    v7a = Objects.requireNonNull(newV7a);
  }

  public java.util.HashMap<String, Integer> getV8() {
    return v8;
  }

  public void setV8(java.util.HashMap<String, Integer> newV8) {
    v8 = Objects.requireNonNull(newV8);
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
      v5a.equals(other.v5a) &&
      v5b.equals(other.v5b) &&
      v6.equals(other.v6) &&
      v7.equals(other.v7) &&
      v7a.equals(other.v7a) &&
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
    result = result * 37 + v5a.hashCode();
    result = result * 37 + v5b.hashCode();
    result = result * 37 + v6.hashCode();
    result = result * 37 + v7.hashCode();
    result = result * 37 + v7a.hashCode();
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
