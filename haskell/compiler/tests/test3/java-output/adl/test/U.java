package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class U {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The U discriminator type.
   */
  public enum Disc {
    F_INT,
    F_STRING
  }

  /* Constructors */

  public static U f_int(short v) {
    return new U(Disc.F_INT, v);
  }

  public static U f_string(String v) {
    return new U(Disc.F_STRING, java.util.Objects.requireNonNull(v));
  }

  public U() {
    this.disc = Disc.F_INT;
    this.value = 0;
  }

  public U(U other) {
    this.disc = other.disc;
    switch (other.disc) {
      case F_INT:
        this.value = (Short) other.value;
        break;
      case F_STRING:
        this.value = (String) other.value;
        break;
    }
  }

  private U(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public short getF_int() {
    if (disc == Disc.F_INT) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  public String getF_string() {
    if (disc == Disc.F_STRING) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setF_int(short v) {
    this.value = v;
    this.disc = Disc.F_INT;
  }

  public void setF_string(String v) {
    this.value = java.util.Objects.requireNonNull(v);
    this.disc = Disc.F_STRING;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof U)) {
      return false;
    }
    U other = (U) other0;
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

  public static final Factory<U> FACTORY = new Factory<U>() {
    public U create() {
      return new U();
    }
    public U create(U other) {
      return new U(other);
    }
  };
}
