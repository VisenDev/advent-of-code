

function part1_scan_for_highest_digit(str, start, count)
    highest_digit = '0'
    highest_digit_i = -1
    for i in range(start, length=count)
        if(str[i] > highest_digit || highest_digit_i == -1)
            highest_digit = str[i]
            highest_digit_i = i
        end
    end
    return highest_digit_i
end

function part1()
    fp = open("3.dat", "r")
    strs = split(read(fp, String), "\n")
    close(fp)

    sum = 0
        
    for str in strs
        
        digit_one_i = part1_scan_for_highest_digit(str, 1, length(str) - 1)
        digit_two_i = part1_scan_for_highest_digit(str, digit_one_i + 1, length(str) - digit_one_i)
        
        num = parse(Int, string(str[digit_one_i]) * string(str[digit_two_i]))
        sum += num
    end
    println(sum)
end


# part 2
function part2_scan_for_highest_digit(str, start, count)
    highest_digit = '0'
    highest_digit_i = -1
    for i in range(start, length=count)
        if(str[i] > highest_digit || highest_digit_i == -1)
            highest_digit = str[i]
            highest_digit_i = i
        end
    end
    return highest_digit_i
end

function part2()
    fp = open("3.dat", "r")
    strs = split(read(fp, String), "\n")
    close(fp)

    sum = 0
        
    for str in strs
        
#        digit_one_i = part1_scan_for_highest_digit(str, 1, length(str) - 12)
#        digit_two_i = part1_scan_for_highest_digit(str, digit_one_i + 1, length(str) - digit_one_i - 11)
#        digit_three_i #etc...

        digit_one_i   = part1_scan_for_highest_digit(str, 1,                    length(str) - 11)
        digit_two_i   = part1_scan_for_highest_digit(str, digit_one_i   + 1,    length(str) - digit_one_i   - 10)
        digit_three_i = part1_scan_for_highest_digit(str, digit_two_i   + 1,    length(str) - digit_two_i   - 9)
        digit_four_i  = part1_scan_for_highest_digit(str, digit_three_i + 1,    length(str) - digit_three_i - 8)
        digit_five_i  = part1_scan_for_highest_digit(str, digit_four_i  + 1,    length(str) - digit_four_i  - 7)
        digit_six_i   = part1_scan_for_highest_digit(str, digit_five_i  + 1,    length(str) - digit_five_i  - 6)
        digit_seven_i = part1_scan_for_highest_digit(str, digit_six_i   + 1,    length(str) - digit_six_i   - 5)
        digit_eight_i = part1_scan_for_highest_digit(str, digit_seven_i + 1,    length(str) - digit_seven_i - 4)
        digit_nine_i  = part1_scan_for_highest_digit(str, digit_eight_i + 1,    length(str) - digit_eight_i - 3)
        digit_ten_i   = part1_scan_for_highest_digit(str, digit_nine_i  + 1,    length(str) - digit_nine_i  - 2)
        digit_eleven_i= part1_scan_for_highest_digit(str, digit_ten_i   + 1,    length(str) - digit_ten_i   - 1)
        digit_twelve_i= part1_scan_for_highest_digit(str, digit_eleven_i+ 1,    length(str) - digit_eleven_i   )

        num = parse(Int, string(str[digit_one_i]) *
            string(str[digit_two_i]) *
            string(str[digit_three_i]) *
            string(str[digit_four_i]) *
            string(str[digit_five_i]) *
            string(str[digit_six_i]) *
            string(str[digit_seven_i]) *
            string(str[digit_eight_i]) *
            string(str[digit_nine_i]) *
            string(str[digit_ten_i]) *
            string(str[digit_eleven_i]) *
            string(str[digit_twelve_i]))

        sum += num
        println(str, " - ", num)
    end
    println(sum)
end







println()
#part1()
part2()
