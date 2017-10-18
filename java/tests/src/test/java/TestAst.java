import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.FileReader;
import java.nio.file.Paths;
import java.nio.file.Path;

import org.adl.runtime.sys.adlast.Module;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.JsonHelpers;

import com.google.gson.Gson;
import com.google.gson.JsonElement;

public class TestAst {

  @Test
  public void testDecodeModule() throws Exception {
    String astPath =  "../../haskell/compiler/tests/test15/ast-output/test.json";
    Module module = JsonHelpers.fromFile(Module.jsonBinding(), astPath);
  }
};
