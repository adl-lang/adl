/* @generated from adl module sys.adlast */

package org.adl.runtime.sys.adlast;

import com.google.gson.JsonElement;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.JsonParseException;
import org.adl.runtime.Lazy;
import java.util.ArrayList;
import java.util.Objects;

public class TypeRef {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The TypeRef discriminator type.
   */
  public enum Disc {
    PRIMITIVE,
    TYPEPARAM,
    REFERENCE
  }

  /* Constructors */

  public static TypeRef primitive(String v) {
    return new TypeRef(Disc.PRIMITIVE, Objects.requireNonNull(v));
  }

  public static TypeRef typeParam(String v) {
    return new TypeRef(Disc.TYPEPARAM, Objects.requireNonNull(v));
  }

  public static TypeRef reference(ScopedName v) {
    return new TypeRef(Disc.REFERENCE, Objects.requireNonNull(v));
  }

  public TypeRef() {
    this.disc = Disc.PRIMITIVE;
    this.value = "";
  }

  public TypeRef(TypeRef other) {
    this.disc = other.disc;
    switch (other.disc) {
      case PRIMITIVE:
        this.value = (String) other.value;
        break;
      case TYPEPARAM:
        this.value = (String) other.value;
        break;
      case REFERENCE:
        this.value = ScopedName.FACTORY.create((ScopedName) other.value);
        break;
    }
  }

  private TypeRef(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public String getPrimitive() {
    if (disc == Disc.PRIMITIVE) {
      return (String) value;
    }
    throw new IllegalStateException();
  }

  public String getTypeParam() {
    if (disc == Disc.TYPEPARAM) {
      return (String) value;
    }
    throw new IllegalStateException();
  }

  public ScopedName getReference() {
    if (disc == Disc.REFERENCE) {
      return (ScopedName) value;
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setPrimitive(String v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.PRIMITIVE;
  }

  public void setTypeParam(String v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.TYPEPARAM;
  }

  public void setReference(ScopedName v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.REFERENCE;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof TypeRef)) {
      return false;
    }
    TypeRef other = (TypeRef) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  /* Factory for construction of generic values */

  public static final Factory<TypeRef> FACTORY = new Factory<TypeRef>() {
    @Override
    public TypeRef create() {
      return new TypeRef();
    }

    @Override
    public TypeRef create(TypeRef other) {
      return new TypeRef(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("sys.adlast", "TypeRef");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(org.adl.runtime.sys.adlast.TypeRef.reference(scopedName), params);
    }

    @Override
    public JsonBinding<TypeRef> jsonBinding() {
      return TypeRef.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<TypeRef> jsonBinding() {
    final Lazy<JsonBinding<String>> primitive = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<String>> typeParam = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<ScopedName>> reference = new Lazy<>(() -> ScopedName.jsonBinding());
    final Factory<TypeRef> _factory = FACTORY;

    return new JsonBinding<TypeRef>() {
      @Override
      public Factory<TypeRef> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(TypeRef _value) {
        switch (_value.getDisc()) {
          case PRIMITIVE:
            return JsonBindings.unionToJson("primitive", _value.getPrimitive(), primitive.get());
          case TYPEPARAM:
            return JsonBindings.unionToJson("typeParam", _value.getTypeParam(), typeParam.get());
          case REFERENCE:
            return JsonBindings.unionToJson("reference", _value.getReference(), reference.get());
        }
        return null;
      }

      @Override
      public TypeRef fromJson(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("primitive")) {
          return TypeRef.primitive(JsonBindings.unionValueFromJson(_json, primitive.get()));
        }
        else if (_key.equals("typeParam")) {
          return TypeRef.typeParam(JsonBindings.unionValueFromJson(_json, typeParam.get()));
        }
        else if (_key.equals("reference")) {
          return TypeRef.reference(JsonBindings.unionValueFromJson(_json, reference.get()));
        }
        throw new JsonParseException("Invalid discriminator " + _key + " for union TypeRef");
      }
    };
  }
}
