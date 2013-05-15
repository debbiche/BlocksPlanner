module ArrayUtils
  def self_or_first(object)
    if object.respond_to? :each
      return object.first
    else
      return object
    end
  end

  def Array.wrap(o)
    o = [o] unless o.respond_to? :each
  end

  def any
    return self_or_first(yield)
  end

  def the
    return self_or_first(yield)
  end

  def all
    return yield
  end
end