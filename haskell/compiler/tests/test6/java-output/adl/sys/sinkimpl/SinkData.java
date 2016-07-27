package adl.sys.sinkimpl;

import org.adl.runtime.Factory;

public class SinkData {

  /* Members */

  private TransportName transport;
  private TransportAddr address;
  private SerialisationType serialisation;

  /* Constructors */

  public SinkData(TransportName transport, TransportAddr address, SerialisationType serialisation) {
    this.transport = java.util.Objects.requireNonNull(transport);
    this.address = java.util.Objects.requireNonNull(address);
    this.serialisation = java.util.Objects.requireNonNull(serialisation);
  }

  public SinkData() {
    this.transport = new TransportName("null");
    this.address = TransportAddr.stringv("");
    this.serialisation = new SerialisationType("json");
  }

  public SinkData(SinkData other) {
    this.transport = TransportName.FACTORY.create(other.transport);
    this.address = TransportAddr.FACTORY.create(other.address);
    this.serialisation = SerialisationType.FACTORY.create(other.serialisation);
  }

  /* Accessors and mutators */

  public TransportName getTransport() {
    return transport;
  }

  public void setTransport(TransportName newTransport) {
    transport = java.util.Objects.requireNonNull(newTransport);
  }

  public TransportAddr getAddress() {
    return address;
  }

  public void setAddress(TransportAddr newAddress) {
    address = java.util.Objects.requireNonNull(newAddress);
  }

  public SerialisationType getSerialisation() {
    return serialisation;
  }

  public void setSerialisation(SerialisationType newSerialisation) {
    serialisation = java.util.Objects.requireNonNull(newSerialisation);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof SinkData)) {
      return false;
    }
    SinkData other = (SinkData) other0;
    return
      transport.equals(other.transport) &&
      address.equals(other.address) &&
      serialisation.equals(other.serialisation);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + transport.hashCode();
    result = result * 37 + address.hashCode();
    result = result * 37 + serialisation.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static final Factory<SinkData> FACTORY = new Factory<SinkData>() {
    public SinkData create() {
      return new SinkData();
    }
    public SinkData create(SinkData other) {
      return new SinkData(other);
    }
  };
}
