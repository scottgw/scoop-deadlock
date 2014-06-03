note
	description: "Summary description for {HTTP_REQUEST}."
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class
	HTTP_REQUEST

create
	make

feature
	make
		do

		end

  proper_method (s : STRING) : BOOLEAN
    do
      Result := s.split (' ').count = 3
    end

	add_method (s : STRING)
		require
			three_parts: proper_method (s)
		local
			splits : LIST [STRING]
		do
			splits     := s.split (' ')

			method     := str_to_method_id (splits.item (1))
			method_uri := splits.item (2)
		end

	add_field  (s : STRING)
		do

		end

	method     : INTEGER
	method_uri : STRING

	get_id  : INTEGER = 0
	head_id : INTEGER = 1

feature {NONE}
	str_to_method_id (s : STRING) : INTEGER
		do
			if s.is_equal ("GET") then
				Result := get_id
			elseif s.is_equal ("HEAD") then
				Result := head_id
			else

			end
		end
end
