mutable struct State 
    knob::Integer
    zero_count::Integer
end

function rotate(s::State, increment::Integer, times::Integer)
    @assert(increment == -1 || increment == 1)
    while(times > 0)
        s.knob += increment
        if(s.knob < 0)
            s.knob = 99
        end
        if(s.knob > 99) 
            s.knob = 0
        end
        if(s.knob == 0) 
            s.zero_count += 1
        end
        times -= 1
    end
end


function main()
    f = open("1.dat", "r")
    strs = split(read(f, String), "\n")
    close(f)

    state = State(50, 0)

    for str in strs
        dir = str[1]
        num = parse(Int32, str[2:end])
        rotate(state, dir == 'L' ? -1 : 1, num)
    end

    print(state.zero_count)
end

main()
