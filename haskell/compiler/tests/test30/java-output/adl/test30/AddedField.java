/* @generated from adl module test30 */

package adl.test30;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Builders;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class AddedField {

  /* Members */

  private double quantity;
  private long value;

  /* Constructors */

  public AddedField(double quantity, long value) {
    this.quantity = quantity;
    this.value = value;
  }

  public AddedField(AddedField other) {
    this.quantity = other.quantity;
    this.value = other.value;
  }

  /* Accessors and mutators */

  public double getQuantity() {
    return quantity;
  }

  public AddedField setQuantity(double quantity) {
    this.quantity = quantity;
    return this;
  }

  public long getValue() {
    return value;
  }

  public AddedField setValue(long value) {
    this.value = value;
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof AddedField)) {
      return false;
    }
    AddedField other = (AddedField) other0;
    return
      quantity == other.quantity &&
      value == other.value;
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + Double.valueOf(quantity).hashCode();
    _result = _result * 37 + (int) (value ^ (value >>> 32));
    return _result;
  }

  /* Builder */

  public static class Builder {
    private Double quantity;
    private Long value;

    public Builder() {
      this.quantity = null;
      this.value = null;
    }

    public Builder setQuantity(Double quantity) {
      this.quantity = Objects.requireNonNull(quantity);
      return this;
    }

    public Builder setValue(Long value) {
      this.value = Objects.requireNonNull(value);
      return this;
    }

    public AddedField create() {
      Builders.checkFieldInitialized("AddedField", "quantity", quantity);
      Builders.checkFieldInitialized("AddedField", "value", value);
      return new AddedField(quantity, value);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<AddedField> FACTORY = new Factory<AddedField>() {
    @Override
    public AddedField create(AddedField other) {
      return new AddedField(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test30", "AddedField");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<AddedField> jsonBinding() {
      return AddedField.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<AddedField> jsonBinding() {
    final Lazy<JsonBinding<Double>> quantity = new Lazy<>(() -> JsonBindings.DOUBLE);
    final Lazy<JsonBinding<Long>> value = new Lazy<>(() -> JsonBindings.INT64);
    final Factory<AddedField> _factory = FACTORY;

    return new JsonBinding<AddedField>() {
      @Override
      public Factory<AddedField> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(AddedField _value) {
        JsonObject _result = new JsonObject();
        _result.add("quantity", quantity.get().toJson(_value.quantity));
        _result.add("value", value.get().toJson(_value.value));
        return _result;
      }

      @Override
      public AddedField fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new AddedField(
          JsonBindings.fieldFromJson(_obj, "quantity", quantity.get()),
          JsonBindings.fieldFromJson(_obj, "value", value.get())
        );
      }
    };
  }
}
