class SearchInSortedRotatedArray {
  def search(nums: Array[Int], target: Int): Int = {
    def searchHelper(low: Int, high: Int, nums: Array[Int], target: Int): Int = {
      if(low > high)
        return -1
      val mid: Int = (low + high) / 2
      if(nums(mid) == target)
        return mid
      if(nums(low) <= nums(mid)){
        if(nums(low) <= target && target <= nums(mid)){
          searchHelper(low, mid-1, nums, target)
        }
        else{
          searchHelper(mid+1, high, nums, target)
        }
      }
      else{
        if(nums(mid) <= target && target <= nums(high)){
          searchHelper(mid+1, high, nums, target)
        }
        else{
          searchHelper(low, mid-1, nums, target)
        }
      }
    }
    val n: Int = nums.size
    searchHelper(0, n-1, nums, target)
  }
}
