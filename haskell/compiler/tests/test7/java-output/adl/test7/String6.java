/* @generated from adl module test7 */

package adl.test7;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
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

public class String6<X> {

  /* Members */

  private String value;

  /* Constructors */

  public String6(String value) {
    this.value = Objects.requireNonNull(value);
  }

  /* Field defaults */

  public static String defValue() {
    return "goodbye";
  }

  /* Accessors and mutators */

  public String getValue() {
    return value;
  }

  public String6<X> setValue(String value) {
    this.value = Objects.requireNonNull(value);
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof String6)) {
      return false;
    }
    String6<?> other = (String6<?>) other0;
    return
      value.equals(other.value);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + value.hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static <X> Factory<String6<X>> factory(Factory<X> factoryX) {
    return new Factory<String6<X>>() {
      final Lazy<Factory<String>> value = new Lazy<>(() -> Factories.STRING);

      @Override
      public String6<X> create(String6<X> other) {
        return new String6<X>(
          other.getValue()
          );
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("test7", "String6");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(factoryX.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }

      @Override
      public JsonBinding<String6<X>> jsonBinding() {
        return String6.jsonBinding(factoryX.jsonBinding());
      }
    };
  }

  /* Json serialization */

  public static<X> JsonBinding<String6<X>> jsonBinding(JsonBinding<X> bindingX) {
    final JsonBinding<String> _binding = JsonBindings.STRING;
    final Factory<String6<X>> _factory = factory(bindingX.factory());

    return new JsonBinding<String6<X>>() {
      @Override
      public Factory<String6<X>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(String6<X> _value) {
        return _binding.toJson(_value.value);
      }

      @Override
      public String6<X> fromJson(JsonElement _json) {
        return new String6<X>(_binding.fromJson(_json));
      }
    };
  }
}
