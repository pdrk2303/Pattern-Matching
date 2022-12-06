import random
import math

#To generate random prime less than N
def randPrime(N):
	primes = []
	for q in range(2,N+1):
		if(isPrime(q)):
			primes.append(q)
	return primes[random.randint(0,len(primes)-1)]

# To check if a number is prime
def isPrime(q):
	if(q > 1):
		for i in range(2, int(math.sqrt(q)) + 1):
			if (q % i == 0):
				return False
		return True
	else:
		return False

#pattern matching
def randPatternMatch(eps,p,x):
	N = findN(eps,len(p))
	q = randPrime(N)
	return modPatternMatch(q,p,x)

#pattern matching with wildcard
def randPatternMatchWildcard(eps,p,x):
	N = findN(eps,len(p))
	q = randPrime(N)
	return modPatternMatchWildcard(q,p,x)

# It is given that if p(N) denote the number of primes that are less than or equal to N,
# then for all N>=1, p(N) >= (N/2log2N)
# if p is the pattern of length m and p' is the m length substring of the text
# there can be a lot occurences where p'!= p but, hash value of p' mod q = hash value of p mod q, where q is a prime number
# we choose q to be a uniformly random prime which is at most an appropriately chosen number N
# where N depends upon the pattern length m and eps which is the upper bound on the error probability
# such that if p=p' then the Pr[p mod q=p' mod q]=1, where Pr is the pobability
# and if p!=p' then the Pr[p mod q=p' mod q]<=eps  
# To find such a N consider x and y to be two m-bit binary numbers, so x,y<pow(2,m) and q to be random prime from the set {1,2,...,N}
# Let d be absolute of their difference, i.e, |x-y|
# Now we say that x = y only when x mod q = y mod q, i.e, (x-y) = 0 mod q instead of directly comparing x and y, which implies q divides d
# The difference d will be a m-bit number, so it will be less than or equal to pow(2,m)
# When d is written as a product of its prime factors as p1*p2*...*pk, where each pi is prime, pi>=2 and some of the primes repeat too
# It is clear that p1*p2*...*pk is greater than or equal to pow(2,k) but less than or equal to pow(2,m) as d<=pow(2,m), which implies k<=m and d has atmost m divisors
# Now the probability that q will be one among the divisors is (m/number of primes in the set {1,2,...,N}),
# as m is the maximum number of prime factors d can have and it is given that it is a prime number from the set {1,2,...,N}
# We know that this probability should be less than or equal to eps for x!=y
# So we have Number of primes in the set {1,2,...,N} is greater than or equal to m/eps
# Also from the theorem mentioned above the number of primes in the set {1,2,...,N} is atleast N/2log2N, i.e, greater than or equal to N/2log2N
# Now there are two possibilities that either m/eps <= N/2log2N or m/eps >= N/2log2N, depending upon the value of eps
# When we simplify we get N <= 2*m*(1/eps)*log2N or N >= 2*m*(1/eps)*log2N
# Therefore we can choose N to be least integer greater than or equal to 2*m*(1/eps)*log2(m*(1/eps))
# And hence we have Pr[x!=y but x mod q=y mod q] <= eps 

# The function findN(eps,m) returns appropriate N that satisfies the error bounds,
# i.e, it returns the least integer greater than or equal to 2*m*(1/eps)*log2(m*(1/eps))
def findN(eps,m):
    M = 2*m*(1/eps)*math.log2(m*(1/eps))
    N = math.ceil(M) 
    return N

y = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']
# Return sorted list of starting indices where p matches x
def modPatternMatch(q,p,x):
    m = len(p) #length of pattern
    n = len(x) # length of text
    pat = 0 # hash value of the pattern and initialising it to 0
    txt = 0 # hash value of the text and initialising it to 0
    h = 1  
    d = 26 # number of characters in the input alphabet
    output = []
    for i in range(m-1):
        h = (h*d) % q
    # value of h would be pow(d,m-1) % q
    
    # calculating the hash values of the pattern(p) and first m characters of the text,i.e, x[0:m]
    for i in range(m):
        pat = (d*pat + y.index(p[i])) % q
        txt = (d*txt + y.index(x[i])) % q  
    # value of pat would be (summation from i=0 to m-1 of pow(26,m-i-1)*y.index(p[i])) % q
    # value of txt would be (summation from i=0 to m-1 of pow(26,m-i-1)*y.index(x[i])) % q
    # loops are run to calculate h, pat and txt instead of directly computing their values,
    # inorder to decrease the time complexity as arithmetic operations on b-bit number takes O(b) time and also to reduce the number of bits in the working memory
    
    # if hash value of pattern mod q is equal to hash value of substring of length m in text mod q
    # that is, hash value of p mod q = hash value of x[i:i+m] mod q
    # then the index i will be appended to the output list
    for i in range(n-m+1):
        if pat == txt:
            output.append(i) 
        if i < n-m:
            # calculating the hash value of the next m-length substring of the text, i.e, x[i+1:m+1]
            # by subtracting the leading alphabet and adding the trailing alphabet
            txt = (d*(txt-y.index(x[i])*h) + y.index(x[i+m])) % q
            # there are possibilities of the hash value being negative so making it positive by adding q
            if txt < 0:
                txt = txt + q
    return output

# Return sorted list of starting indices where p matches x
def modPatternMatchWildcard(q,p,x):
    m = len(p) #length of pattern
    n = len(x) # length of text
    pat = 0 # hash value of the pattern and initialising it to 0
    txt = 0 # hash value of the text and initialising it to 0
    h = 1  
    h1 = 1
    h2 = 1
    d = 26 # number of characters in the input alphabet
    output = []
    # pattern contains exactly one occurence of the wildcard
    # finding the index where the wildcard character occurs in the pattern
    for i in range(m):
        if p[i] == '?': 
            wildcard = i
            break
    for i in range(m-1):
        h = (h*d) % q
    # value of h would be pow(d,m-1) % q
    for i in range(m-wildcard-1):
        h1 = (h1*d) % q
    # value of h1 would be pow(d,m-wildcard-1) % q
    for i in range(m-wildcard-2):
        h2 = (h2*d) % q
    # value of h2 would be pow(d,m-wildcard-2) % q
    
    # calculating the hash values of the pattern(p) and first m characters of the text,i.e, x[0:m]
    # except that the index where the wildcard appears is skipped that is 
    # instead of adding the corresponding index of the alphabet, 0 is added
    # this is done because when there is a wildcard character in the pattern, there can be any alphabet at that position in the substring of the text 
    # but the rest all characters of the pattern should match with that of the substring of the text
    for i in range(m):
        if i!=wildcard:
            pat = (d*pat + y.index(p[i])) % q
            txt = (d*txt + y.index(x[i])) % q     
        else:
            pat = (d*pat) % q
            txt = (d*txt) % q
    # value of pat would be (summation from i=0 to m-1 and i!=wildcard of pow(26,m-i-1)*y.index(p[i])) % q
    # value of txt would be (summation from i=0 to m-1 and i!=wildcard of pow(26,m-i-1)*y.index(x[i])) % q
    # loops are run to calculate h, h1, h2 pat and txt instead of directly computing their values,
    # inorder to decrease the time complexity as arithmetic operations on b-bit number takes O(b) time and also to reduce the number of bits in the working memory

    # if hash value of pattern mod q is equal to hash value of substring of length m in text mod q
    # that is, hash value of p mod q = hash value of x[i:i+m] mod q
    # then the index i will be appended to the output list  
    for i in range(n-m+1): 
        if pat == txt:
            output.append(i)
        if i < n-m:
            # there are three possibilities for the value that wildcard takes
            # wildcard = 0 or wildcard = m-1 or wildcard can be any interior index
            # now to calculate the hash value for next window of text, it is not correct to just subtract the leading alphabet and adding the trailing alphabet
            # when wildcard is an interior index, i.e, 0<wildcard<m-1
            # the hash value of the next m-length substring of the text, i.e, x[i+1:m+1] can be calculated by 
            # subtracting the leading digit, adding the alphabet occuring at the wildcard position in the substring x[i:m] of the text,
            # subtracting the alphabet occuring at the wildcard position in the substring x[i+1:m+1] of the text 
            # which will be the alphabet next to the alphabet in the wildcard postion in the substring x[i:i+m] and finally adding the trailing alphabet
            if wildcard>0 and wildcard<(m-1):
                txt = (d*(txt-y.index(x[i])*h+y.index(x[i+wildcard])*h1-y.index(x[i+wildcard+1])*h2) + y.index(x[i+m])) % q 
            # when wildcard index is m-1
            # the hash value of the next m-length substring of the text, i.e, x[i+1:m+1] can be calculated by 
            # subtracting the leading digit, adding the alphabet occuring at the wildcard position in the substring x[i:m] of the text
            # there is no need to add the trailing alphabet as that would be the wildcard position and we are skipping that index to compare the hash values of the pattern and the text
            elif wildcard==m-1:
                txt = (d*(txt-y.index(x[i])*h+y.index(x[i+wildcard])*h1)) % q
            # when wildcard index is 0
            # the hash value of the next m-length substring of the text, i.e, x[i+1:m+1] can be calculated by 
            # subtracting the alphabet occuring at the wildcard position in the substring x[i+1:m+1] of the text 
            # which will be the alphabet next to the alphabet in the wildcard postion in the substring x[i:i+m] and adding the trailing alphabet
            # there would be no need to subtract the leading alphabet because since that is the position where the wildcard character appears in the pattern
            # and that index would have been already skipped while calculating the hash value initially
            elif wildcard==0:
                txt = (d*(txt-y.index(x[i+wildcard+1])*h2) + y.index(x[i+m])) % q
            # there are possibilities of the hash value being negative so making it positive by adding q
            if txt < 0:
                txt = txt + q 
    return output


