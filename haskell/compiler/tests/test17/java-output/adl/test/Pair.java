package adl.test;

import org.adl.runtime.Factory;

public class Pair<A, B> {

  /* Members */

  private A v1;
  private B v2;

  /* Constructors */

  public Pair(A v1, B v2) {
    this.v1 = java.util.Objects.requireNonNull(v1);
    this.v2 = java.util.Objects.requireNonNull(v2);
  }

  /* Accessors and mutators */

  public A getV1() {
    return v1;
  }

  public void setV1(A newV1) {
    v1 = newV1;
  }

  public B getV2() {
    return v2;
  }

  public void setV2(B newV2) {
    v2 = newV2;
  }

  /* Object level helpers */

  public boolean equals(Pair other) {
    return
      v1.equals(other.v1) &&
      v2.equals(other.v2);
  }

  public int hashCode() {
    int result = 1;
    result = result * 37 + v1.hashCode();
    result = result * 37 + v2.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static <A, B> Factory<Pair<A, B>> factory(Factory<A> factoryA, Factory<B> factoryB) {
    return new Factory<Pair<A, B>>() {
      final Factory<A> v1 = factoryA;
      final Factory<B> v2 = factoryB;

      public Pair<A, B> create() {
        return new Pair<A, B>(v1.create(), v2.create());
      }

      public Pair<A, B> create(Pair<A, B> other) {
        return new Pair<A, B>(v1.create(other.getV1()), v2.create(other.getV2()));
      }
    };
  }
}