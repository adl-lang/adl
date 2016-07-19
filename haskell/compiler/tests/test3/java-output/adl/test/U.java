package adl.test;

import org.adl.runtime.Factories;

public class U {

  private Disc disc;
  private Object value;

  public enum Disc {
    F_INT,
    F_STRING
  }

  public static U f_int(short v) {
    return new U(Disc.F_INT,v);
  }

  public static U f_string(String v) {
    return new U(Disc.F_STRING,java.util.Objects.requireNonNull(v));
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

  public Disc getDisc() {
    return disc;
  }
}