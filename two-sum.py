def two_sum(numbers, target):
    nums_covered = set()
    for index, num in enumerate(numbers):
        if target-num in nums_covered:
            break
        else:
            nums_covered.add(num)
    
    for index2, num2 in enumerate(numbers):
        if num2 == target-num and index != index2:
            return [index2+1, index+1]
