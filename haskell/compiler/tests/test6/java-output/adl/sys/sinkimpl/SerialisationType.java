package adl.sys.sinkimpl;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class SerialisationType {

  /* Members */

  private String value;

  /* Constructors */

  public SerialisationType(String value) {
    this.value = java.util.Objects.requireNonNull(value);
  }

  public SerialisationType() {
    this.value = "";
  }

  public SerialisationType(SerialisationType other) {
    this.value = other.value;
  }

  /* Accessors and mutators */

  public String getValue() {
    return value;
  }

  public void setValue(String newValue) {
    value = java.util.Objects.requireNonNull(newValue);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof SerialisationType)) {
      return false;
    }
    SerialisationType other = (SerialisationType) other0;
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

  public static final Factory<SerialisationType> FACTORY = new Factory<SerialisationType>() {
    public SerialisationType create() {
      return new SerialisationType();
    }
    public SerialisationType create(SerialisationType other) {
      return new SerialisationType(other);
    }
  };
}
