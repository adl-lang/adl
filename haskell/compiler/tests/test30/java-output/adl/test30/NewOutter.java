/* @generated from adl module test30 */

package adl.test30;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class NewOutter {

  /* Members */

  private Lifted field0;

  /* Constructors */

  public NewOutter(Lifted field0) {
    this.field0 = Objects.requireNonNull(field0);
  }

  public NewOutter(NewOutter other) {
    this.field0 = Lifted.FACTORY.create(other.field0);
  }

  /* Accessors and mutators */

  public Lifted getField0() {
    return field0;
  }

  public NewOutter setField0(Lifted field0) {
    this.field0 = Objects.requireNonNull(field0);
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof NewOutter)) {
      return false;
    }
    NewOutter other = (NewOutter) other0;
    return
      field0.equals(other.field0);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + field0.hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static final Factory<NewOutter> FACTORY = new Factory<NewOutter>() {
    @Override
    public NewOutter create(NewOutter other) {
      return new NewOutter(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test30", "NewOutter");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<NewOutter> jsonBinding() {
      return NewOutter.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<NewOutter> jsonBinding() {
    final Lazy<JsonBinding<Lifted>> field0 = new Lazy<>(() -> Lifted.jsonBinding());
    final Factory<NewOutter> _factory = FACTORY;

    return new JsonBinding<NewOutter>() {
      @Override
      public Factory<NewOutter> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(NewOutter _value) {
        JsonObject _result = new JsonObject();
        _result.add("field0", field0.get().toJson(_value.field0));
        return _result;
      }

      @Override
      public NewOutter fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new NewOutter(
          JsonBindings.fieldFromJson(_obj, "field0", field0.get())
        );
      }
    };
  }
}
