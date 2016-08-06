package adl.sys.types;

import org.adl.runtime.Factory;
import java.util.Objects;

public class Pair<T1, T2> {

  /* Members */

  private T1 v1;
  private T2 v2;

  /* Constructors */

  public Pair(T1 v1, T2 v2) {
    this.v1 = Objects.requireNonNull(v1);
    this.v2 = Objects.requireNonNull(v2);
  }

  /* Accessors and mutators */

  public T1 getV1() {
    return v1;
  }

  public void setV1(T1 newV1) {
    v1 = Objects.requireNonNull(newV1);
  }

  public T2 getV2() {
    return v2;
  }

  public void setV2(T2 newV2) {
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

  public static <T1, T2> Factory<Pair<T1, T2>> factory(Factory<T1> factoryT1, Factory<T2> factoryT2) {
    return new Factory<Pair<T1, T2>>() {
      final Factory<T1> v1 = factoryT1;
      final Factory<T2> v2 = factoryT2;

      public Pair<T1, T2> create() {
        return new Pair<T1, T2>(v1.create(), v2.create());
      }

      public Pair<T1, T2> create(Pair<T1, T2> other) {
        return new Pair<T1, T2>(v1.create(other.getV1()), v2.create(other.getV2()));
      }
    };
  }
}
