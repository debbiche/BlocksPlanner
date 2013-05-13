class World
  PREPOSITIONS = ["leftof", "under", "rightof", "ontop", "beside", "above"]
  attr_reader :world, :blocks, :grabber
  def initialize(world = nil, grabber = nil)
    @grabber = grabber
    @world = world || [[], ["a","b"], ["c","d"], [], ["e","f","g","h","i"], [], [], ["j","k"], [], ["l","m"]];
    @blocks = {
    "a" =>  { "form" => "rectangle", "size" => "tall",   "color" => "blue",   "width" => 0.50, "height" => 1.00 },
    "b" =>  { "form" => "ball",      "size" => "small",  "color" => "white",  "width" => 0.50, "height" => 0.50 },
    "c" =>  { "form" => "square",    "size" => "large",  "color" => "red",    "width" => 1.00, "height" => 1.00 },
    "d" =>  { "form" => "pyramid",   "size" => "large",  "color" => "green",  "width" => 1.00, "height" => 1.00 },
    "e" =>  { "form" => "box",       "size" => "large",  "color" => "white",  "width" => 1.00, "height" => 0.75 },
    "f" =>  { "form" => "rectangle", "size" => "wide",   "color" => "black",  "width" => 1.00, "height" => 0.50 },
    "g" =>  { "form" => "rectangle", "size" => "wide",   "color" => "blue",   "width" => 1.00, "height" => 0.50 },
    "h" =>  { "form" => "rectangle", "size" => "wide",   "color" => "red",    "width" => 1.00, "height" => 0.50 },
    "i" =>  { "form" => "pyramid",   "size" => "medium", "color" => "yellow", "width" => 0.75, "height" => 0.75 },
    "j" =>  { "form" => "box",       "size" => "large",  "color" => "red",    "width" => 1.00, "height" => 0.75 },
    "k" =>  { "form" => "ball",      "size" => "small",  "color" => "yellow", "width" => 0.50, "height" => 0.50 },
    "l" =>  { "form" => "box",       "size" => "medium", "color" => "red",    "width" => 0.75, "height" => 0.50 },
    "m" =>  { "form" => "ball",      "size" => "medium", "color" => "blue",   "width" => 0.75, "height" => 0.75 }
    }
  end

  def with_properties(args)
    @blocks.select do |name, block|
      match = true
      args.each do |key, value|
        match = (block[key.to_s] == value) && match unless value == "_"
      end
      match
    end.keys
  end

  def position_of(block_name)
    column_index = column_index_of(block_name)
    return nil unless column_index
    height = get_column(column_index).index {|row| row == block_name}
    [column_index, height]
  end

  def get_column(column_index)
    return @world[column_index]
  end

  def column_index_of(block_name)
    column_index = @world.index {|col| col.include? block_name}
  end

  def with_preposition(preposition, block_name)
    if PREPOSITIONS.include? preposition
      self.send(preposition, block_name)
    end
  end

  def remove_block_from_column(block_name, column_index) 
    get_column(column_index).reject! {|block| block == block_name}
    return block_name
  end

  def remove_block(block_name)
    column_index = column_index_of(block_name)
    remove_block_from_column(block_name, column_index)
  end

  def top_of_column(column_index)
    column = get_column(column_index)
    if column
      return column.size
    else
      return nil
    end
  end

  def put_on_top_of_column(block, column_index)
    get_column(column_index).push(block)
  end

  # Horizontal operations

  def leftof_position(block_name)
    calculate_column(position_of(block_name), -1)
  end
  
  def rightof_position(block_name)
    calculate_column(position_of(block_name), 1)
  end

  def beside_position(block_name)
    leftof_position(block_name) || rightof_position(block_name)
  end

  def calculate_column(origin, diff)
    new_column = origin[0] + diff
    position = [new_column, top_of_column(new_column)]
    if sane(position)
      return position
    else
      return nil
    end
  end

  ["leftof", "rightof", "beside"].each do |position|
    define_method position do |subject, target|
      remove_block(subject)
      target_position = self.send("#{position}_position", target)
      insert_block_at_position(subject, target_position)
    end
  end


  # Vertical operations

  def ontop(subject, target)
    remove_block(subject)
    target_block_position  = position_of(target)
    column = target_block_position[0]
    row = target_block_position[1] + 1
    insert_block_at_position(subject, [column, row])
  end

  def above(subject, target)
    remove_block(subject)
    column_index = column_index_of(target)
    put_on_top_of_column(subject, column_index)
  end

  def insert_block_at_position(block, position)
    if sane(position)
      column_index = position[0]
      row_index = position[1]
      column = get_column(column_index)
      column.insert(row_index, block)
    end
  end

  # Sanity checks

  def sane(position)
    position[0] >= 0 && position[1] >= 0 && position[0] < @world.size
  end
end