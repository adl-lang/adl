package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class U {

  private short f_int;
  private String f_string;

  public U(short f_int, String f_string) {
    this.f_int = f_int;
    this.f_string = java.util.Objects.requireNonNull(f_string);
  }

  public U() {
    this.f_int = 0;
    this.f_string = "";
  }

  public U(U other) {
    this.f_int = other.f_int;
    this.f_string = other.f_string;
  }

  public short getF_int() {
    return f_int;
  }

  public void setF_int(short newF_int) {
    f_int = newF_int;
  }

  public String getF_string() {
    return f_string;
  }

  public void setF_string(String newF_string) {
    f_string = newF_string;
  }

  public boolean equals(U other) {
    return
      f_int == other.f_int &&
      f_string.equals(other.f_string);
  }

  public int hashCode() {
    int result = 1;
    result = result * 37 + (int)f_int;
    result = result * 37 + f_string.hashCode();
    return result;
  }

  public static Factory<U> factory = new Factory<U>() {
    public U create() {
      return new U();
    }
    public U create(U other) {
      return new U(other);
    }
  };
}