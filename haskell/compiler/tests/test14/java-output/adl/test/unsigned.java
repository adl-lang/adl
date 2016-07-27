package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class unsigned {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The unsigned discriminator type.
   */
  public enum Disc {
    NULL_
  }

  /* Constructors */

  public static unsigned null_() {
    return new unsigned(Disc.NULL_, null);
  }

  public unsigned() {
    this.disc = Disc.NULL_;
    this.value = null;
  }

  public unsigned(unsigned other) {
    this.disc = other.disc;
    switch (other.disc) {
      case NULL_:
        this.value = (Void) other.value;
        break;
    }
  }

  private unsigned(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  /* Mutators */

  public void setNull() {
    this.value = null;
    this.disc = Disc.NULL_;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof unsigned)) {
      return false;
    }
    unsigned other = (unsigned) other0;
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

  public static final Factory<unsigned> FACTORY = new Factory<unsigned>() {
    public unsigned create() {
      return new unsigned();
    }
    public unsigned create(unsigned other) {
      return new unsigned(other);
    }
  };
}
