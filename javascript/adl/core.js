// standard adl functions available for every ADL type:
//
//   ADLTYPE.validate(v) :
//     confirms that the value v is valid for the given ADL type
//     return null if ok, otherwise an error object, with
//        `error` : the (first) error
//        `context` : the path to the (first) erroneous element
//
//   ADLTYPE.defaultv() :
//     Create a new value of the specified ADL type, initialised
//     with the ADL defined defaults.
//
//   ADLTYPE.copy(v) :
//     Creates a deep copy of the ADL value v.
//
//   ADLTYPE.equals(v1, v2) :
//     Performs a deep comparison of the ADL value v1, and
//     ADL value v2.
//
//   ADLTYPE.fromJson(jv) :
//     Converts the json value jv into a value of the given ADL type.
//     This will throw an exception of type AdlParseError if the
//     json value is invalid (see the [ADL json specification][1])
//
//   ADLTYPE.toJson(jv) :
//     Converts the ADL value v into a json value.
//
// Generally the structure of ADL values in javascript is consistent
// with the [ADL json specification][1]. However, the javascript values
// must have all fields present, and may also custom representations
// specified via appropriate [custom type attributes][2].
//
// [1]: https://github.com/timbod7/adl/blob/master/doc/serialization.md
// [2]: https://github.com/timbod7/adl/blob/master/doc/custom-types.md

function AdlError(message) {
  this.name = 'AdlError';
  this.message = message;
  this.context = [];
}
AdlError.prototype = Object.create(Error.prototype);
AdlError.prototype.constructor = AdlError;

function expectedError(v, typestr) {
  return new AdlError("Expected " + typestr + ", found " + typeof(v));
}

function validatePrimitive(v, typestr) {
  if (typeof(v) == typestr) {
    return null;
  } else {
    return expectedError(v, typestr);
  }
}

//----------------------------------------------------------------------

function Void() {
  function validate(v) {
    if (v === null) {
      return null;
    } else {
      return expectedError(v,"null");
    }
  };
  
  function defaultv() {
    return null;
  };

  function copy(v) {
    return v;
  };

  function equals(v1, v2) {
    return v1 == v2;
  };
  
  function fromJson(jv) {
    if (v === null) {
      return null;
    } else {
      throw new expectedError(v,"null");
    }
  };

  function toJson(v) {
    return v;
  };

  return {
    validate : validate,
    defaultv : defaultv,
    copy : copy,
    equals : equals,
    fromJson : fromJson,
    toJson : toJson
  };
}

exports.Void = Void;

//----------------------------------------------------------------------

function Double() {
  function validate(v) {
    return validatePrimitive( v, "number" );
  };
  
  function defaultv() {
    return null;
  };

  function copy(v) {
    return v;
  };

  function equals(v1, v2) {
    return v1 == v2;
  };
  
  function fromJson(jv) {
    var err = validatePrimitive( jv, "number" );
    if (err) {
      throw err;
    }
    return jv;
  };

  function toJson(v) {
    return v;
  };

  return {
    validate : validate,
    defaultv : defaultv,
    copy : copy,
    equals : equals,
    fromJson : fromJson,
    toJson : toJson
  };
}

exports.Double = Double;

//----------------------------------------------------------------------

exports.Vector = function(type) {
  function validate(v) {
    if (!Array.isArray(v)) {
      return expectedError(v, "array");
    }
    for(var i = 0; i < v.length; i++) {
      var err = type.validate(v[i]);
      if ( err ) {
        err.context.push(i);
        return err;
      }
    }
    return null;
  };

  function defaultv() {
    return [];
  }

  function copy(v) {
    return v.map(type.copy);
  }
  
  function equals(v1,v2) {
    if (v1.length != v2.length) {
      return false;
    }
    for(var i = 0; i < v1.length; i++) {
      if (type.equals(v1[i], v2[i])) {
        return false;
      }
    }
    return true;
  }

  function fromJson(jv) {
    if (!Array.isArray(v)) {
      throw expectedError(v, "array");
    }

    var result = [];
    for (var i = 0; i < jv.length; i++) {
      try {
        result.push( type.fromJson(jv[i]) );
      } catch(err) {
        err.context.push(i);
        throw err;
      }
    }
    return result;
  }

  function toJson(v) {
    return v.map( type.toJson );
  }
  
  return {
    validate : validate,
    defaultv : defaultv,
    copy : copy,
    equals : equals,
    fromJson : fromJson,
    toJson : toJson
  };
};
  
//----------------------------------------------------------------------

exports.UnionType = function(name,fields) {
  var fieldDict = {};
  fields.forEach( (field) => {
    fieldDict[field[0]] = field[1];
  });
  var disc0 = fields[0][0];
  
  return {
    name : name,
    kind : "union",
    fields : fields,
    
    validate : function(v) {
      if (typeof(v) == "string") {
        if (v in fieldDict) {
          var fieldType = fieldDict[v]();
          if (fieldType == exports.Void) {
            return null;
          } else {
            return new AdlError(v + " is not a void discriminator");
          }
        } else {
          return new AdlError(v + " is not a union discriminator");
        }
      } else if (typeof(v) == "object") {
        for(var disc in v) {
          if (disc in fieldDict) {
            var err = fieldDict[disc]().validate(v[disc]);
            if(err) {
              err.context.push(disc);
            }
            return err;
          } else {
            return new AdlError(disc + " is not a discriminator");
          }
        }
        return new AdlError("union is missing a discriminator");
      }
      return expectedError(v, "object or string");
    },
    default : function () {
      var fieldType0 = fieldDict[disc0]();
      if (fieldType0 == exports.Void) {
        return disc0;
      } else {
        var result = {};
        result[disc0] = fieldType0.default();
        return result;
      }
    },
    copy : function(v) {
      if (typeof(v) == "string") {
        return v;
      }
      for (var disc in v) {
        var result = {};
        result[disc] = fieldDict[disc]().copy(v[disc]);
        return result;
      }
    },
    equals : function(v1, v2) {
      if (typeof(v1) == "string") {
        return v1 == v2;
      }
      var disc1 = null;
      for (disc1 in v1) { break; }
      var disc2 = null;
      for (disc2 in v2) { break; }
      if (disc1 != disc2) {
        return false;
      }
      return fieldDict[disc1].equals(v1[disc1], v2[disc2]);
    },
    fromJson : function(jv) {
      if (typeof(jv) == 'string') {
        return jv;
      } else {
        for (var disc in jv) {
          var result = {};
          result[disc] = fieldDict[disc]().fromJson(jv[disc]);
          return result;
        }
      }
    },
    toJson : function(v) {
      if (typeof(v) == 'string') {
        return v;
      } else {
        for (var disc in v) {
          var result = {};
          result[disc] = fieldDict[disc]().toJson(v[disc]);
          return result;
        }
      }
    }
  };
};

//----------------------------------------------------------------------

exports.StructType = function(name,fields) {
  var fieldDict = {};
  fields.forEach( (field) => {
    fieldDict[field[0]] = field[1];
  });
  
  return {
    name : name,
    kind : "struct",
    fields : fields,
    
    validate : function(v) {
      if (typeof(v) != "object") {
        return expectedError(v, "object");
      }
      if (v == null) {
        return new AdlError("expected object, found null");
      }
      for (var fieldName in fieldDict) {
        if (!(fieldName in v)) {
          return new AdlError("struct is missing field " + fieldName);
        }
        var err = fieldDict[fieldName]().validate(v[fieldName]);
        if (err != null) {
          err.context.push(fieldName);
          return err;
        }
      }
      return null;
    },
    default : function () {
      var result = {};
      for(var name in fieldDict) {
        result[name] = fieldDict[name]().default();
      }
      return result;
    },
    copy : function(v) {
      var result = {};
      for(var name in fieldDict) {
        result[name] = fieldDict[name]().copy(v[name]);
      }
      return result;
    },
    equals : function(v1, v2) {
      for(var name in fieldDict) {
        if (!fieldDict[name]().equals(v1[name], v2[name])) {
          return false;
        }
      }
      return true;
    },
    fromJson : function(jv) {
      var result = {};
      for (var name in fieldDict) {
        if (name in jv) {
          result[name] = fieldDict[name]().fromJson(jv[name]);
        } else {
          result[name] = fieldDict[name]().default();
        }
      }
      return result;
    },
    toJson : function(v) {
      // FIXME
    }
  };
};
