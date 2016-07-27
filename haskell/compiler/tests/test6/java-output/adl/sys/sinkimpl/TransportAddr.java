package adl.sys.sinkimpl;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class TransportAddr {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The TransportAddr discriminator type.
   */
  public enum Disc {
    STRINGV,
    INTV,
    ARRAYV
  }

  /* Constructors */

  public static TransportAddr stringv(String v) {
    return new TransportAddr(Disc.STRINGV, java.util.Objects.requireNonNull(v));
  }

  public static TransportAddr intv(long v) {
    return new TransportAddr(Disc.INTV, v);
  }

  public static TransportAddr arrayv(java.util.ArrayList<TransportAddr> v) {
    return new TransportAddr(Disc.ARRAYV, java.util.Objects.requireNonNull(v));
  }

  public TransportAddr() {
    this.disc = Disc.STRINGV;
    this.value = "";
  }

  public TransportAddr(TransportAddr other) {
    this.disc = other.disc;
    switch (other.disc) {
      case STRINGV:
        this.value = (String) other.value;
        break;
      case INTV:
        this.value = (Long) other.value;
        break;
      case ARRAYV:
        this.value = Factories.arrayList(TransportAddr.FACTORY).create((java.util.ArrayList<TransportAddr>) other.value);
        break;
    }
  }

  private TransportAddr(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public String getStringv() {
    if (disc == Disc.STRINGV) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  public long getIntv() {
    if (disc == Disc.INTV) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  public java.util.ArrayList<TransportAddr> getArrayv() {
    if (disc == Disc.ARRAYV) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setStringv(String v) {
    this.value = java.util.Objects.requireNonNull(v);
    this.disc = Disc.STRINGV;
  }

  public void setIntv(long v) {
    this.value = v;
    this.disc = Disc.INTV;
  }

  public void setArrayv(java.util.ArrayList<TransportAddr> v) {
    this.value = java.util.Objects.requireNonNull(v);
    this.disc = Disc.ARRAYV;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof TransportAddr)) {
      return false;
    }
    TransportAddr other = (TransportAddr) other0;
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

  public static final Factory<TransportAddr> FACTORY = new Factory<TransportAddr>() {
    public TransportAddr create() {
      return new TransportAddr();
    }
    public TransportAddr create(TransportAddr other) {
      return new TransportAddr(other);
    }
  };
}
