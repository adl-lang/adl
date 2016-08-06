package adl.test;

import org.adl.runtime.Factories;
import java.util.Objects;

public class Factory {

  /* Members */

  private String value;

  /* Constructors */

  public Factory(String value) {
    this.value = Objects.requireNonNull(value);
  }

  public Factory() {
    this.value = "";
  }

  public Factory(Factory other) {
    this.value = other.value;
  }

  /* Accessors and mutators */

  public String getValue() {
    return value;
  }

  public void setValue(String newValue) {
    value = Objects.requireNonNull(newValue);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Factory)) {
      return false;
    }
    Factory other = (Factory) other0;
    return
      value.equals(other.value);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + value.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static final org.adl.runtime.Factory<Factory> FACTORY = new org.adl.runtime.Factory<Factory>() {
    public Factory create() {
      return new Factory();
    }
    public Factory create(Factory other) {
      return new Factory(other);
    }
  };
}
