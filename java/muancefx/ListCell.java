package muancefx;

import jdk.nashorn.api.scripting.ScriptObjectMirror;

public class ListCell extends javafx.scene.control.ListCell {
  private ScriptObjectMirror factoryFn = null;
  private Object f = null;
  private Object vtree = null;

  public ListCell(ScriptObjectMirror factoryFn, Object vtree, Object f) {
    this.factoryFn = factoryFn;
    this.vtree = vtree;
    this.f = f;
  }

  @Override
  protected void updateItem(Object item, boolean empty) {
    super.updateItem(item, empty);
    this.factoryFn.callMember("call", null, this, this.vtree, this.f, item, empty);
  }
}
