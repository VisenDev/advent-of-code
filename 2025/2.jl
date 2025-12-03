
function is_id_valid_part1(id::String)::Bool
    if (mod(length(id), 2) == 1)
        return true
    end

    midpoint::Integer = length(id) / 2
    s1 = id[1:midpoint]
    s2 = id[midpoint + 1:end]
    if(s1 == s2)
        return false
    else
        return true
    end
end

function part1()
    fp = open("2.dat", "r")
    range_strs = split(read(fp, String), ",")
    close(fp)

    sum = 0

    for range_str in range_strs
        range_vec = split(range_str, "-")
        start_range = parse(Int, range_vec[1])
        end_range = parse(Int, range_vec[2])
        for i in start_range:end_range
            
            if(!is_id_valid_part1(string(i)))
                sum += i
            end
        end
    end
    println()
    println(sum)
end





# ==== Part 2 ====
function is_id_repeating_string(id::String, str::String)
    teststr = ""
    while(length(teststr) < length(id))
        teststr *= str
    end
    return teststr == id
end

function is_id_valid_part2(id::String)::Bool
    digits::Integer = 1
    while(digits < length(id))
        substr = id[1:digits]
        digits += 1
        if(is_id_repeating_string(id, substr))
            return false
        end
    end
    return true
end

function part2()
    fp = open("2.dat", "r")
    range_strs = split(read(fp, String), ",")
    close(fp)

    sum = 0

    for range_str in range_strs
        range_vec = split(range_str, "-")
        start_range = parse(Int, range_vec[1])
        end_range = parse(Int, range_vec[2])
        for i in start_range:end_range
            if(!is_id_valid_part2(string(i)))
                sum += i
#                println(i)
            end
        end
    end
    println()
    println(sum)
end

part1()
part2()



