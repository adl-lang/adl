package adl.test14;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class switch_ {

  /* Members */

  private double double_;
  private int int_;
  private String string;
  private boolean for_;
  private String Objects;

  /* Constructors */

  public switch_(double double_, int int_, String string, boolean for_, String Objects) {
    this.double_ = double_;
    this.int_ = int_;
    this.string = java.util.Objects.requireNonNull(string);
    this.for_ = for_;
    this.Objects = java.util.Objects.requireNonNull(Objects);
  }

  public switch_() {
    this.double_ = 0.0;
    this.int_ = 0;
    this.string = "";
    this.for_ = false;
    this.Objects = "";
  }

  public switch_(switch_ other) {
    this.double_ = other.double_;
    this.int_ = other.int_;
    this.string = other.string;
    this.for_ = other.for_;
    this.Objects = other.Objects;
  }

  /* Accessors and mutators */

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
    string = java.util.Objects.requireNonNull(newString);
  }

  public boolean getFor() {
    return for_;
  }

  public void setFor(boolean newFor) {
    for_ = newFor;
  }

  public String getObjects() {
    return Objects;
  }

  public void setObjects(String newObjects) {
    Objects = java.util.Objects.requireNonNull(newObjects);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof switch_)) {
      return false;
    }
    switch_ other = (switch_) other0;
    return
      double_ == other.double_ &&
      int_ == other.int_ &&
      string.equals(other.string) &&
      for_ == other.for_ &&
      Objects.equals(other.Objects);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + Double.valueOf(double_).hashCode();
    result = result * 37 + int_;
    result = result * 37 + string.hashCode();
    result = result * 37 + (for_ ? 0 : 1);
    result = result * 37 + Objects.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static final Factory<switch_> FACTORY = new Factory<switch_>() {
    public switch_ create() {
      return new switch_();
    }
    public switch_ create(switch_ other) {
      return new switch_(other);
    }
  };
}
