package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class U2 {

  private Disc disc;
  private Object value;

  public enum Disc {
    V
  }

  public static U2 v(short v) {
    return new U2(Disc.V,v);
  }

  public U2() {
    this.disc = Disc.V;
    this.value = 0;
  }

  public U2(U2 other) {
    this.disc = other.disc;
    switch (other.disc) {
      case V:
        this.value = (Short) other.value;
        break;
    }
  }

  private U2(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  public Disc getDisc() {
    return disc;
  }

  public short getV() {
    if (disc == Disc.V) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  public void setV(short v) {
    this.value = v;
    this.disc = Disc.V;
  }

  public boolean equals(U2 other) {
    return disc == other.disc && value.equals(other.value);
  }

  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  @SuppressWarnings("unchecked")
  private static <T> T cast(final Object o) {
    return (T)o;
  }

  public static Factory<U2> factory = new Factory<U2>() {
    public U2 create() {
      return new U2();
    }
    public U2 create(U2 other) {
      return new U2(other);
    }
  };
}