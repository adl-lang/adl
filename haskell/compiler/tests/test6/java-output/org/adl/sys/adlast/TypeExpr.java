/* Code generated from adl module sys.adlast */

package org.adl.sys.adlast;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import java.util.ArrayList;
import java.util.Objects;

@SuppressWarnings("unused")
public class TypeExpr {

  /* Members */

  private TypeRef typeRef;
  private ArrayList<TypeExpr> parameters;

  /* Constructors */

  public TypeExpr(TypeRef typeRef, ArrayList<TypeExpr> parameters) {
    this.typeRef = Objects.requireNonNull(typeRef);
    this.parameters = Objects.requireNonNull(parameters);
  }

  public TypeExpr() {
    this.typeRef = new TypeRef();
    this.parameters = new ArrayList<TypeExpr>();
  }

  public TypeExpr(TypeExpr other) {
    this.typeRef = TypeRef.FACTORY.create(other.typeRef);
    this.parameters = Factories.arrayList(TypeExpr.FACTORY).create(other.parameters);
  }

  /* Accessors and mutators */

  public TypeRef getTypeRef() {
    return typeRef;
  }

  public void setTypeRef(TypeRef typeRef) {
    this.typeRef = Objects.requireNonNull(typeRef);
  }

  public ArrayList<TypeExpr> getParameters() {
    return parameters;
  }

  public void setParameters(ArrayList<TypeExpr> parameters) {
    this.parameters = Objects.requireNonNull(parameters);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof TypeExpr)) {
      return false;
    }
    TypeExpr other = (TypeExpr) other0;
    return
      typeRef.equals(other.typeRef) &&
      parameters.equals(other.parameters);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + typeRef.hashCode();
    _result = _result * 37 + parameters.hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static final Factory<TypeExpr> FACTORY = new Factory<TypeExpr>() {
    @Override
    public TypeExpr create() {
      return new TypeExpr();
    }

    @Override
    public TypeExpr create(TypeExpr other) {
      return new TypeExpr(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<TypeExpr> jsonBinding() {
    final Lazy<JsonBinding<TypeRef>> typeRef = new Lazy<>(() -> TypeRef.jsonBinding());
    final Lazy<JsonBinding<ArrayList<TypeExpr>>> parameters = new Lazy<>(() -> JsonBindings.arrayList(org.adl.sys.adlast.TypeExpr.jsonBinding()));
    final Factory<TypeExpr> _factory = FACTORY;

    return new JsonBinding<TypeExpr>() {
      public Factory<TypeExpr> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(TypeExpr _value) {
        JsonObject _result = new JsonObject();
        _result.add("typeRef", typeRef.get().toJson(_value.typeRef));
        _result.add("parameters", parameters.get().toJson(_value.parameters));
        return _result;
      }

      @Override
      public TypeExpr fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new TypeExpr(
          JsonBindings.fieldFromJson(_obj, "typeRef", typeRef.get()),
          JsonBindings.fieldFromJson(_obj, "parameters", parameters.get())
        );
      }
    };
  }
}
