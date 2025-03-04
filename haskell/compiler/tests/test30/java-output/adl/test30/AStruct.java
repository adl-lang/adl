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

public class AStruct {

  /* Members */

  private BStruct b;

  /* Constructors */

  public AStruct(BStruct b) {
    this.b = Objects.requireNonNull(b);
  }

  public AStruct() {
    this.b = defB();
  }

  public AStruct(AStruct other) {
    this.b = BStruct.FACTORY.create(other.b);
  }

  /* Field defaults */

  public static BStruct defB() {
    return new BStruct("abc $10");
  }

  /* Accessors and mutators */

  public BStruct getB() {
    return b;
  }

  public AStruct setB(BStruct b) {
    this.b = Objects.requireNonNull(b);
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof AStruct)) {
      return false;
    }
    AStruct other = (AStruct) other0;
    return
      b.equals(other.b);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + b.hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static final Factory<AStruct> FACTORY = new Factory<AStruct>() {
    @Override
    public AStruct create(AStruct other) {
      return new AStruct(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test30", "AStruct");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<AStruct> jsonBinding() {
      return AStruct.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<AStruct> jsonBinding() {
    final Lazy<JsonBinding<BStruct>> b = new Lazy<>(() -> BStruct.jsonBinding());
    final Factory<AStruct> _factory = FACTORY;

    return new JsonBinding<AStruct>() {
      @Override
      public Factory<AStruct> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(AStruct _value) {
        JsonObject _result = new JsonObject();
        _result.add("b", b.get().toJson(_value.b));
        return _result;
      }

      @Override
      public AStruct fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new AStruct(
          _obj.has("b") ? JsonBindings.fieldFromJson(_obj, "b", b.get()) : new BStruct("abc $10")
        );
      }
    };
  }
}
