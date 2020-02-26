import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.sql.*;
import DatabaseDriver.*;

public class UniversalGUI implements ActionListener
{
	JFrame jframe;
	DatabaseDriver dd = new DatabaseDriver();

	void initFrame()
	{
		jframe = new JFrame("Universal GUI");
		jframe.setSize(new Dimension(500,500));
	}

	

	public UniversalGUI()
	{
		initFrame();
	}


	public void actionPerformed(ActionEvent ae)
	{
		if(ae.getActionCommand().equals("Open"))
		{
			try
			{
				dd.open(databasename.getText());
			}
			catch(Exception e)
			{
				System.out.println(e);
			}
		}
		

	}
	public static void main(String args[])throws Exception
	{
		new UniversalGUI();
	}

}