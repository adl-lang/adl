package org.adl.test4;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import java.util.Objects;

public class Date {

  /* Members */

  private String value;

  /* Constructors */

  public Date(String value) {
    this.value = Objects.requireNonNull(value);
  }

  public Date() {
    this.value = "1900-01-01";
  }

  public Date(Date other) {
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
    if (!(other0 instanceof Date)) {
      return false;
    }
    Date other = (Date) other0;
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

  public static final Factory<Date> FACTORY = new Factory<Date>() {
    public Date create() {
      return new Date();
    }
    public Date create(Date other) {
      return new Date(other);
    }
  };
}
