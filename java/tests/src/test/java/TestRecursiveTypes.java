import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import adl.test2.Tree;
import adl.test5.List;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;

public class TestRecursiveTypes {

  @Test
  public void testArrayRecursionFactory(){
    Factory<Tree<String>> factory = Tree.factory(Factories.STRING);
  }

  @Test
  public void testArrayRecursionJson(){
    JsonBinding<Tree<String>> jbinding = Tree.jsonBinding(JsonBindings.STRING);
  }

  @Test
  public void testUnionRecursionFactory(){
    Factory<List<String>> factory = List.factory(Factories.STRING);
  }

  @Test
  public void testUnionRecursionJson(){
    JsonBinding<List<String>> jbinding = List.jsonBinding(JsonBindings.STRING);
  }

};
