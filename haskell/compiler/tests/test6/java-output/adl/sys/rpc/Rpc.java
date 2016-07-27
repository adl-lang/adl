package adl.sys.rpc;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class Rpc<I, O> {

  /* Members */

  private I params;
  private Sink<O> replyTo;

  /* Constructors */

  public Rpc(I params, Sink<O> replyTo) {
    this.params = java.util.Objects.requireNonNull(params);
    this.replyTo = java.util.Objects.requireNonNull(replyTo);
  }

  /* Accessors and mutators */

  public I getParams() {
    return params;
  }

  public void setParams(I newParams) {
    params = java.util.Objects.requireNonNull(newParams);
  }

  public Sink<O> getReplyTo() {
    return replyTo;
  }

  public void setReplyTo(Sink<O> newReplyTo) {
    replyTo = java.util.Objects.requireNonNull(newReplyTo);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Rpc)) {
      return false;
    }
    Rpc other = (Rpc) other0;
    return
      params.equals(other.params) &&
      replyTo.equals(other.replyTo);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + params.hashCode();
    result = result * 37 + replyTo.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static <I, O> Factory<Rpc<I, O>> factory(Factory<I> factoryI, Factory<O> factoryO) {
    return new Factory<Rpc<I, O>>() {
      final Factory<I> params = factoryI;
      final Factory<Sink<O>> replyTo = Factories.SINK(factoryO);

      public Rpc<I, O> create() {
        return new Rpc<I, O>(params.create(), replyTo.create());
      }

      public Rpc<I, O> create(Rpc<I, O> other) {
        return new Rpc<I, O>(params.create(other.getParams()), replyTo.create(other.getReplyTo()));
      }
    };
  }
}
