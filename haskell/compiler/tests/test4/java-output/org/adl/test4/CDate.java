package org.adl.test4;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import java.util.Objects;

public class CDate {

  /* Members */

  private short year;
  private short month;
  private short day;

  /* Constructors */

  public CDate(short year, short month, short day) {
    this.year = year;
    this.month = month;
    this.day = day;
  }

  public CDate() {
    this.year = 0;
    this.month = 0;
    this.day = 0;
  }

  public CDate(CDate other) {
    this.year = other.year;
    this.month = other.month;
    this.day = other.day;
  }

  /* Accessors and mutators */

  public short getYear() {
    return year;
  }

  public void setYear(short newYear) {
    year = newYear;
  }

  public short getMonth() {
    return month;
  }

  public void setMonth(short newMonth) {
    month = newMonth;
  }

  public short getDay() {
    return day;
  }

  public void setDay(short newDay) {
    day = newDay;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof CDate)) {
      return false;
    }
    CDate other = (CDate) other0;
    return
      year == other.year &&
      month == other.month &&
      day == other.day;
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + (int) year;
    result = result * 37 + (int) month;
    result = result * 37 + (int) day;
    return result;
  }

  /* Factory for construction of generic values */

  public static final Factory<CDate> FACTORY = new Factory<CDate>() {
    public CDate create() {
      return new CDate();
    }
    public CDate create(CDate other) {
      return new CDate(other);
    }
  };
}
