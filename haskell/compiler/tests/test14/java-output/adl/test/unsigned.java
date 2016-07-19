package adl.test;

import org.adl.runtime.Factories;

public class unsigned {

  private Disc disc;
  private Object value;

  public enum Disc {
    NULL_
  }

  public static unsigned null_(Void v) {
    return new unsigned(Disc.NULL_,v);
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

  public Disc getDisc() {
    return disc;
  }
}