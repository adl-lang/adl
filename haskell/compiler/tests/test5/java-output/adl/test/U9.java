package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class U9<T> {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The U9 discriminator type.
   */
  public enum Disc {
    V1,
    V2
  }

  /* Constructors */

  public static <T> U9 v1(T v) {
    return new U9(Disc.V1, java.util.Objects.requireNonNull(v));
  }

  public static <T> U9 v2(short v) {
    return new U9(Disc.V2, v);
  }

  private U9(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public T getV1() {
    if (disc == Disc.V1) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  public short getV2() {
    if (disc == Disc.V2) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setV1(T v) {
    this.value = java.util.Objects.requireNonNull(v);
    this.disc = Disc.V1;
  }

  public void setV2(short v) {
    this.value = v;
    this.disc = Disc.V2;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof U9)) {
      return false;
    }
    U9 other = (U9) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  @SuppressWarnings("unchecked")
  private static <T> T cast(final Object o) {
    return (T) o;
  }

  /* Factory for construction of generic values */

  public static <T> Factory<U9 <T>> factory(Factory<T> factoryT) {
    return new Factory<U9<T>>() {
      final Factory<T> v1 = factoryT;

      public U9<T> create() {
        return new U9<T>(Disc.V1,v1.create());
      }

      public U9<T> create(U9<T> other) {
        switch (other.disc) {
          case V1:
            return new U9<T>(other.disc,v1.create(cast(other.value)));
          case V2:
            return new U9<T>(other.disc,other.value);
        }
        throw new IllegalArgumentException();
      }
    };
  }
}
