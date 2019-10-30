/* @generated from adl module sys.adlast */

package org.adl.runtime.sys.adlast;

import com.google.gson.JsonElement;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.JsonParseException;
import org.adl.runtime.Lazy;
import java.util.ArrayList;
import java.util.Objects;

public class DeclType {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The DeclType discriminator type.
   */
  public enum Disc {
    STRUCT_,
    UNION_,
    TYPE_,
    NEWTYPE_
  }

  /* Constructors */

  public static DeclType struct_(Struct v) {
    return new DeclType(Disc.STRUCT_, Objects.requireNonNull(v));
  }

  public static DeclType union_(Union v) {
    return new DeclType(Disc.UNION_, Objects.requireNonNull(v));
  }

  public static DeclType type_(TypeDef v) {
    return new DeclType(Disc.TYPE_, Objects.requireNonNull(v));
  }

  public static DeclType newtype_(NewType v) {
    return new DeclType(Disc.NEWTYPE_, Objects.requireNonNull(v));
  }

  public DeclType() {
    this.disc = Disc.STRUCT_;
    this.value = new Struct();
  }

  public DeclType(DeclType other) {
    this.disc = other.disc;
    switch (other.disc) {
      case STRUCT_:
        this.value = Struct.FACTORY.create((Struct) other.value);
        break;
      case UNION_:
        this.value = Union.FACTORY.create((Union) other.value);
        break;
      case TYPE_:
        this.value = TypeDef.FACTORY.create((TypeDef) other.value);
        break;
      case NEWTYPE_:
        this.value = NewType.FACTORY.create((NewType) other.value);
        break;
    }
  }

  private DeclType(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public Struct getStruct_() {
    if (disc == Disc.STRUCT_) {
      return (Struct) value;
    }
    throw new IllegalStateException();
  }

  public Union getUnion_() {
    if (disc == Disc.UNION_) {
      return (Union) value;
    }
    throw new IllegalStateException();
  }

  public TypeDef getType_() {
    if (disc == Disc.TYPE_) {
      return (TypeDef) value;
    }
    throw new IllegalStateException();
  }

  public NewType getNewtype_() {
    if (disc == Disc.NEWTYPE_) {
      return (NewType) value;
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setStruct_(Struct v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.STRUCT_;
  }

  public void setUnion_(Union v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.UNION_;
  }

  public void setType_(TypeDef v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.TYPE_;
  }

  public void setNewtype_(NewType v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.NEWTYPE_;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof DeclType)) {
      return false;
    }
    DeclType other = (DeclType) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  /* Factory for construction of generic values */

  public static final Factory<DeclType> FACTORY = new Factory<DeclType>() {
    @Override
    public DeclType create() {
      return new DeclType();
    }

    @Override
    public DeclType create(DeclType other) {
      return new DeclType(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("sys.adlast", "DeclType");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }

    @Override
    public JsonBinding<DeclType> jsonBinding() {
      return DeclType.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<DeclType> jsonBinding() {
    final Lazy<JsonBinding<Struct>> struct_ = new Lazy<>(() -> Struct.jsonBinding());
    final Lazy<JsonBinding<Union>> union_ = new Lazy<>(() -> Union.jsonBinding());
    final Lazy<JsonBinding<TypeDef>> type_ = new Lazy<>(() -> TypeDef.jsonBinding());
    final Lazy<JsonBinding<NewType>> newtype_ = new Lazy<>(() -> NewType.jsonBinding());
    final Factory<DeclType> _factory = FACTORY;

    return new JsonBinding<DeclType>() {
      @Override
      public Factory<DeclType> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(DeclType _value) {
        switch (_value.getDisc()) {
          case STRUCT_:
            return JsonBindings.unionToJson("struct_", _value.getStruct_(), struct_.get());
          case UNION_:
            return JsonBindings.unionToJson("union_", _value.getUnion_(), union_.get());
          case TYPE_:
            return JsonBindings.unionToJson("type_", _value.getType_(), type_.get());
          case NEWTYPE_:
            return JsonBindings.unionToJson("newtype_", _value.getNewtype_(), newtype_.get());
        }
        return null;
      }

      @Override
      public DeclType fromJson(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("struct_")) {
          return DeclType.struct_(JsonBindings.unionValueFromJson(_json, struct_.get()));
        }
        else if (_key.equals("union_")) {
          return DeclType.union_(JsonBindings.unionValueFromJson(_json, union_.get()));
        }
        else if (_key.equals("type_")) {
          return DeclType.type_(JsonBindings.unionValueFromJson(_json, type_.get()));
        }
        else if (_key.equals("newtype_")) {
          return DeclType.newtype_(JsonBindings.unionValueFromJson(_json, newtype_.get()));
        }
        throw new JsonParseException("Invalid discriminator " + _key + " for union DeclType");
      }
    };
  }
}
