note
	description: "Summary description for {MIME_GUESSER}."
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class
	MIME_GUESSER

create
	make

feature
	make
		do
		end

  intern_table : HASH_TABLE [STRING, STRING]

  mime_table : HASH_TABLE [STRING, STRING]
    do
      if intern_table = Void then
        create_mime_table
      end

      Result := intern_table
    end

	create_mime_table
		local
			file : PLAIN_TEXT_FILE
		do
			create intern_table.make (100)
			create file.make_open_read ("mime.types")

			from
			until
				file.end_of_file
			loop
				file.read_line
				process_line (intern_table, file.last_string)
			end
		end

	process_line (table : HASH_TABLE [STRING, STRING]; s : STRING)
		local
			parts : LIST [STRING]
			val   : STRING
			rest  : STRING
			i     : INTEGER
		do
			if not s.starts_with ("#") then
				parts := s.split ('%T')
				if parts.count > 1 then
					val := parts.item (1)

					from
						rest := Void
						i := 2
					until
						i > parts.count or else rest /= Void
					loop
						if not parts.item (i).is_equal ("") then
							rest := parts.item (i)
						end
						i := i + 1
					end

					if rest /= Void then
						parts := rest.split (' ')
						from
							i:= 1
						until
							i > parts.count
						loop
							table.put (val, parts.item (i))
							i := i + 1
						end
					end
				end
			end
		end
end
