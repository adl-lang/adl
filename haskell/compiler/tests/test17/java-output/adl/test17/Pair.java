package adl.test17;

import org.adl.runtime.Factory;
import java.util.Objects;

public class Pair<A, B> {

  /* Members */

  private A v1;
  private B v2;

  /* Constructors */

  public Pair(A v1, B v2) {
    this.v1 = Objects.requireNonNull(v1);
    this.v2 = Objects.requireNonNull(v2);
  }

  /* Accessors and mutators */

  public A getV1() {
    return v1;
  }

  public void setV1(A newV1) {
    v1 = Objects.requireNonNull(newV1);
  }

  public B getV2() {
    return v2;
  }

  public void setV2(B newV2) {
    v2 = Objects.requireNonNull(newV2);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Pair)) {
      return false;
    }
    Pair other = (Pair) other0;
    return
      v1.equals(other.v1) &&
      v2.equals(other.v2);
  }

  @Override
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
        return new Pair<A, B>(
          v1.create(),
          v2.create()
          );
      }

      public Pair<A, B> create(Pair<A, B> other) {
        return new Pair<A, B>(
          v1.create(other.getV1()),
          v2.create(other.getV2())
          );
      }
    };
  }
}
