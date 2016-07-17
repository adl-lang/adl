package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class unsigned {

  private Void null_;

  public unsigned(Void null_) {
    this.null_ = null_;
  }

  public unsigned() {
    this.null_ = null;
  }

  public unsigned(unsigned other) {
    this.null_ = other.null_;
  }

  public Void getNull() {
    return null_;
  }

  public void setNull(Void newNull) {
    null_ = newNull;
  }

  public boolean equals(unsigned other) {
    return
      null_.equals(other.null_);
  }

  public int hashCode() {
    int result = 1;
    result = result * 37 + 0;
    return result;
  }

  public static Factory<unsigned> factory = new Factory<unsigned>() {
    public unsigned create() {
      return new unsigned();
    }
    public unsigned create(unsigned other) {
      return new unsigned(other);
    }
  };
}