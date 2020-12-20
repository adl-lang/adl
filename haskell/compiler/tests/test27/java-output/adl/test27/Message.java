/* @generated from adl module test27 */

package adl.test27;

import adl.test27a.Message2;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Builders;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class Message {

  /* Members */

  private adl.test27a.Message other;
  private Message2 other2;

  /* Constructors */

  public Message(adl.test27a.Message other, Message2 other2) {
    this.other = Objects.requireNonNull(other);
    this.other2 = Objects.requireNonNull(other2);
  }

  public Message(Message other) {
    this.other = adl.test27a.Message.FACTORY.create(other.other);
    this.other2 = Message2.FACTORY.create(other.other2);
  }

  /* Accessors and mutators */

  public adl.test27a.Message getOther() {
    return other;
  }

  public Message setOther(adl.test27a.Message other) {
    this.other = Objects.requireNonNull(other);
    return this;
  }

  public Message2 getOther2() {
    return other2;
  }

  public Message setOther2(Message2 other2) {
    this.other2 = Objects.requireNonNull(other2);
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Message)) {
      return false;
    }
    Message other = (Message) other0;
    return
      other.equals(other.other) &&
      other2.equals(other.other2);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + other.hashCode();
    _result = _result * 37 + other2.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder {
    private adl.test27a.Message other;
    private Message2 other2;

    public Builder() {
      this.other = null;
      this.other2 = null;
    }

    public Builder setOther(adl.test27a.Message other) {
      this.other = Objects.requireNonNull(other);
      return this;
    }

    public Builder setOther2(Message2 other2) {
      this.other2 = Objects.requireNonNull(other2);
      return this;
    }

    public Message create() {
      Builders.checkFieldInitialized("Message", "other", other);
      Builders.checkFieldInitialized("Message", "other2", other2);
      return new Message(other, other2);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<Message> FACTORY = new Factory<Message>() {
    @Override
    public Message create(Message other) {
      return new Message(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test27", "Message");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<Message> jsonBinding() {
      return Message.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<Message> jsonBinding() {
    final Lazy<JsonBinding<adl.test27a.Message>> other = new Lazy<>(() -> adl.test27a.Message.jsonBinding());
    final Lazy<JsonBinding<Message2>> other2 = new Lazy<>(() -> Message2.jsonBinding());
    final Factory<Message> _factory = FACTORY;

    return new JsonBinding<Message>() {
      @Override
      public Factory<Message> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Message _value) {
        JsonObject _result = new JsonObject();
        _result.add("other", other.get().toJson(_value.other));
        _result.add("other2", other2.get().toJson(_value.other2));
        return _result;
      }

      @Override
      public Message fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new Message(
          JsonBindings.fieldFromJson(_obj, "other", other.get()),
          JsonBindings.fieldFromJson(_obj, "other2", other2.get())
        );
      }
    };
  }
}
