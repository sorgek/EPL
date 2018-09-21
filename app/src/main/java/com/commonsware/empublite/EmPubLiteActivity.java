package com.commonsware.empublite;

import android.app.Activity;
import android.app.ListActivity;
import android.os.Bundle;
import android.view.View;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.TextView;

public class EmPubLiteActivity extends ListActivity {

/*  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.main);
  }*/

  private TextView selection;
  private static final String[] items={"lorem", "ipsum", "dolor",
          "sit", "amet",
          "consectetuer", "adipiscing", "elit", "morbi", "vel",
          "ligula", "vitae", "arcu", "aliquet", "mollis",
          "etiam", "vel", "erat", "placerat", "ante",
          "porttitor", "sodales", "pellentesque", "augue", "purus"};



  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.btex);
    setListAdapter(new ArrayAdapter<String>(this,
            android.R.layout.simple_list_item_1,
            items));
    selection = (TextView) findViewById(R.id.selection);

  }
  @Override
    public void onListItemClick(ListView parent, View v, int position, long id){
      selection.setText(items[position]);
  }
}
