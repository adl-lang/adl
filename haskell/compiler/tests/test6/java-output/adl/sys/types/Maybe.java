package adl.sys.types;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class Maybe<T> {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The Maybe discriminator type.
   */
  public enum Disc {
    NOTHING,
    JUST
  }

  /* Constructors */

  public static <T> Maybe nothing() {
    return new Maybe(Disc.NOTHING, null);
  }

  public static <T> Maybe just(T v) {
    return new Maybe(Disc.JUST, java.util.Objects.requireNonNull(v));
  }

  private Maybe(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public T getJust() {
    if (disc == Disc.JUST) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setNothing() {
    this.value = null;
    this.disc = Disc.NOTHING;
  }

  public void setJust(T v) {
    this.value = java.util.Objects.requireNonNull(v);
    this.disc = Disc.JUST;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Maybe)) {
      return false;
    }
    Maybe other = (Maybe) other0;
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

  public static <T> Factory<Maybe <T>> factory(Factory<T> factoryT) {
    return new Factory<Maybe<T>>() {
      final Factory<T> just = factoryT;

      public Maybe<T> create() {
        return new Maybe<T>(Disc.NOTHING,null);
      }

      public Maybe<T> create(Maybe<T> other) {
        Object value = null;
        switch (other.disc) {
          case NOTHING:
            value = other.value;
            break;
          case JUST:
            value = just.create(cast(other.value));
            break;
        }
        return new Maybe<T>(other.disc,value);
      }
    };
  }
}
