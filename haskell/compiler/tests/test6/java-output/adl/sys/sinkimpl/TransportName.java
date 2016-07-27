package adl.sys.sinkimpl;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class TransportName {

  /* Members */

  private String value;

  /* Constructors */

  public TransportName(String value) {
    this.value = java.util.Objects.requireNonNull(value);
  }

  public TransportName() {
    this.value = "";
  }

  public TransportName(TransportName other) {
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
    if (!(other0 instanceof TransportName)) {
      return false;
    }
    TransportName other = (TransportName) other0;
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

  public static final Factory<TransportName> FACTORY = new Factory<TransportName>() {
    public TransportName create() {
      return new TransportName();
    }
    public TransportName create(TransportName other) {
      return new TransportName(other);
    }
  };
}
