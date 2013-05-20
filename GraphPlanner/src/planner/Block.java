package planner;

public class Block {
	private String id;
	private String shape = null;
	private String color = null;
	private int size = 0;
	
	public Block(String id, String shape, String color, String size) {
		super();
		this.id = id;
		this.shape = shape;
		this.color = color;
		int aux = 0;
		if(size.equals("small")) aux = 1;
		if(size.equals("medium")) aux = 2;
		if(size.equals("large")) aux = 3;
		if(size.equals("tall")) aux = 1;
		if(size.equals("wide")) aux = 3;
		this.size = aux;
	}
	
	public boolean compareTo(Object arg0) {
		if ((this.id.equals(((Block) arg0).id)) && 
		   (this.shape.equals(((Block) arg0).shape)) && 
		   (this.color.equals(((Block) arg0).color)) && 
		   (this.size == ((Block) arg0).size)) return true;
		return false;
	}
	
	public boolean isSpecialShape() {
		return this.shape.equals("ball")||this.shape.equals("pyramid");
	}
	
	public boolean fitsInside(Block b) {
		return (this.size<=b.size);
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getShape() {
		return shape;
	}

	public void setShape(String shape) {
		this.shape = shape;
	}

	public String getColor() {
		return color;
	}

	public void setColor(String color) {
		this.color = color;
	}

	public int getSize() {
		return size;
	}

	public void setSize(int size) {
		this.size = size;
	}

	@Override
	public String toString() {
		return "Block [id=" + id + ", shape=" + shape + ", color=" + color
				+ ", size=" + size + "]";
	}
	
}
