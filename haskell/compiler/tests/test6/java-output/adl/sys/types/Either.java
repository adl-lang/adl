package adl.sys.types;

import org.adl.runtime.Factory;

public class Either<T1, T2> {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The Either discriminator type.
   */
  public enum Disc {
    LEFT,
    RIGHT
  }

  /* Constructors */

  public static <T1, T2> Either left(T1 v) {
    return new Either(Disc.LEFT, java.util.Objects.requireNonNull(v));
  }

  public static <T1, T2> Either right(T2 v) {
    return new Either(Disc.RIGHT, java.util.Objects.requireNonNull(v));
  }

  private Either(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public T1 getLeft() {
    if (disc == Disc.LEFT) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  public T2 getRight() {
    if (disc == Disc.RIGHT) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setLeft(T1 v) {
    this.value = java.util.Objects.requireNonNull(v);
    this.disc = Disc.LEFT;
  }

  public void setRight(T2 v) {
    this.value = java.util.Objects.requireNonNull(v);
    this.disc = Disc.RIGHT;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Either)) {
      return false;
    }
    Either other = (Either) other0;
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

  public static <T1, T2> Factory<Either <T1, T2>> factory(Factory<T1> factoryT1, Factory<T2> factoryT2) {
    return new Factory<Either<T1, T2>>() {
      final Factory<T1> left = factoryT1;
      final Factory<T2> right = factoryT2;

      public Either<T1, T2> create() {
        return new Either<T1, T2>(Disc.LEFT,left.create());
      }

      public Either<T1, T2> create(Either<T1, T2> other) {
        switch (other.disc) {
          case LEFT:
            return new Either<T1, T2>(other.disc,left.create(cast(other.value)));
          case RIGHT:
            return new Either<T1, T2>(other.disc,right.create(cast(other.value)));
        }
        throw new IllegalArgumentException();
      }
    };
  }
}
