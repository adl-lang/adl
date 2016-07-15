package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class switch_ {

  private double double_;
  private int int_;
  private String string;
  private boolean for_;

  public switch_(double double_, int int_, String string, boolean for_) {
    this.double_ = double_;
    this.int_ = int_;
    this.string = java.util.Objects.requireNonNull(string);
    this.for_ = for_;
  }

  public switch_() {
    this.double_ = 0;
    this.int_ = 0;
    this.string = "";
    this.for_ = false;
  }

  public switch_(switch_ other) {
    this.double_ = other.double_;
    this.int_ = other.int_;
    this.string = other.string;
    this.for_ = other.for_;
  }

  public double getDouble() {
    return double_;
  }

  public void setDouble(double newDouble) {
    double_ = newDouble;
  }

  public int getInt() {
    return int_;
  }

  public void setInt(int newInt) {
    int_ = newInt;
  }

  public String getString() {
    return string;
  }

  public void setString(String newString) {
    string = newString;
  }

  public boolean getFor() {
    return for_;
  }

  public void setFor(boolean newFor) {
    for_ = newFor;
  }

  public boolean equals(switch_ other) {
    return
      double_ == other.double_ &&
      int_ == other.int_ &&
      string.equals(other.string) &&
      for_ == other.for_;
  }

  public int hashCode() {
    int result = 1;
    result = result * 37 + (int)double_;
    result = result * 37 + (int)int_;
    result = result * 37 + string.hashCode();
    result = result * 37 + (for_ ? 0 : 1);
    return result;
  }

  public static Factory<switch_> factory = new Factory<switch_>() {
    public switch_ create() {
      return new switch_();
    }
    public switch_ create(switch_ other) {
      return new switch_(other);
    }
  };
}