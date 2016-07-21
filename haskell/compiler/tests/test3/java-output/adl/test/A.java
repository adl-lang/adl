package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class A {

  /* Members */

  private short f_int;
  private String f_string;
  private boolean f_bool;

  /* Constructors */

  public A(short f_int, String f_string, boolean f_bool) {
    this.f_int = f_int;
    this.f_string = java.util.Objects.requireNonNull(f_string);
    this.f_bool = f_bool;
  }

  public A() {
    this.f_int = 0;
    this.f_string = "";
    this.f_bool = false;
  }

  public A(A other) {
    this.f_int = other.f_int;
    this.f_string = other.f_string;
    this.f_bool = other.f_bool;
  }

  /* Accessors and mutators */

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

  public boolean getF_bool() {
    return f_bool;
  }

  public void setF_bool(boolean newF_bool) {
    f_bool = newF_bool;
  }

  /* Object level helpers */

  public boolean equals(A other) {
    return
      f_int == other.f_int &&
      f_string.equals(other.f_string) &&
      f_bool == other.f_bool;
  }

  public int hashCode() {
    int result = 1;
    result = result * 37 + (int)f_int;
    result = result * 37 + f_string.hashCode();
    result = result * 37 + (f_bool ? 0 : 1);
    return result;
  }

  /* Factory for construction of generic values */

  public static Factory<A> factory = new Factory<A>() {
    public A create() {
      return new A();
    }
    public A create(A other) {
      return new A(other);
    }
  };
}