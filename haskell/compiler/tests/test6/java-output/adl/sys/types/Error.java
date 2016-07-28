package adl.sys.types;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class Error<T> {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The Error discriminator type.
   */
  public enum Disc {
    VALUE,
    ERROR
  }

  /* Constructors */

  public static <T> Error value(T v) {
    return new Error(Disc.VALUE, java.util.Objects.requireNonNull(v));
  }

  public static <T> Error error(String v) {
    return new Error(Disc.ERROR, java.util.Objects.requireNonNull(v));
  }

  private Error(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public T getValue() {
    if (disc == Disc.VALUE) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  public String getError() {
    if (disc == Disc.ERROR) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setValue(T v) {
    this.value = java.util.Objects.requireNonNull(v);
    this.disc = Disc.VALUE;
  }

  public void setError(String v) {
    this.value = java.util.Objects.requireNonNull(v);
    this.disc = Disc.ERROR;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Error)) {
      return false;
    }
    Error other = (Error) other0;
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

  public static <T> Factory<Error <T>> factory(Factory<T> factoryT) {
    return new Factory<Error<T>>() {
      final Factory<T> value = factoryT;

      public Error<T> create() {
        return new Error<T>(Disc.VALUE,value.create());
      }

      public Error<T> create(Error<T> other) {
        switch (other.disc) {
          case VALUE:
            return new Error<T>(other.disc,value.create(cast(other.value)));
          case ERROR:
            return new Error<T>(other.disc,other.value);
        }
        throw new IllegalArgumentException();
      }
    };
  }
}
