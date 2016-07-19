package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class S1 {

  private short f;

  public S1(short f) {
    this.f = f;
  }

  public S1() {
    this.f = (short)100;
  }

  public S1(S1 other) {
    this.f = other.f;
  }

  public short getF() {
    return f;
  }

  public void setF(short newF) {
    f = newF;
  }

  public boolean equals(S1 other) {
    return
      f == other.f;
  }

  public int hashCode() {
    int result = 1;
    result = result * 37 + (int)f;
    return result;
  }

  public static Factory<S1> factory = new Factory<S1>() {
    public S1 create() {
      return new S1();
    }
    public S1 create(S1 other) {
      return new S1(other);
    }
  };
}