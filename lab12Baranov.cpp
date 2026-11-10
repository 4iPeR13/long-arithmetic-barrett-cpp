#include <iostream>
#include <vector>
#include <string>
#include <stdint.h>
#include <stdlib.h>
#include <chrono>
using namespace std;


static const int NUMBER_LENGTH = 64;

class LongNumber {
public:
    std::vector<uint32_t> number_array;

    LongNumber();
    LongNumber(const LongNumber& B);
    LongNumber(const uint32_t number);
    LongNumber(const std::string hex);

    static LongNumber zero();
    static LongNumber one();

    LongNumber operator + (LongNumber& B) const;
    LongNumber operator - (LongNumber& B) const;

    LongNumber operator * (uint32_t B) const;
    LongNumber operator * (LongNumber& B) const;

    LongNumber operator / (LongNumber& B) const;
    LongNumber operator % (LongNumber& B) const;

    LongNumber power(LongNumber& B) const;

    int BitLength() const;
    int DigitLength() const;
    int size() const;
    LongNumber LongShiftDigitsToLow(int shift) const;
    LongNumber LongShiftDigitsToHigh(int shift) const;

    LongNumber LongShiftBitsToHigh(int shift) const;
    LongNumber LongShiftBitsToLow(int shift) const;

    bool operator < (LongNumber& B) const;
    bool operator > (LongNumber& B) const;
    bool operator <= (LongNumber& B) const;
    bool operator >= (LongNumber& B) const;

    bool operator == (LongNumber& const B) const;
    bool operator != (LongNumber& B) const;

    static LongNumber GCD(LongNumber& A, LongNumber& B);
    static LongNumber LCM(LongNumber& A, LongNumber& B);

    friend std::ostream& operator<<(std::ostream& os, const LongNumber& LongNumber);
    void normalize()
    {
        if (size() > 0)
        {
            while (number_array.back() == 0 && number_array.size() > 1)
            {
                number_array.pop_back();
            }
        }
    }

};

uint32_t ConvertHexToDigit(char hex);
char ConvertDigitToHex(uint32_t digit);
LongNumber ConvertToNumber(std::string hex);
std::string ConvertToHex(LongNumber number);


class LongNumberModulo {
private:
    LongNumber modulo;
    LongNumber mu;
    int digitLength;
public:
    LongNumberModulo(LongNumber& mod);

    LongNumber add(LongNumber& A, LongNumber& B);
    LongNumber sub(LongNumber& A, LongNumber& B);

    LongNumber mult(LongNumber& A, uint32_t& B);
    LongNumber mult(LongNumber& A, LongNumber& B);

    LongNumber square(LongNumber& A);

    LongNumber power(LongNumber& A, LongNumber& power);

    LongNumber Barrett(LongNumber& A);
    LongNumber Mu(LongNumber& mod);
};
int LongNumber::size() const {
    return number_array.size();
}
LongNumber::LongNumber() {
    number_array.resize(NUMBER_LENGTH);
    for (int i = 0; i < NUMBER_LENGTH; i++) {
        number_array[i] = 0;
    }
}

LongNumber::LongNumber(const LongNumber& B) {
    number_array.resize(NUMBER_LENGTH);
    for (int i = 0; i < NUMBER_LENGTH; i++) {
        number_array[i] = B.number_array[i];
    }
}

LongNumber::LongNumber(const uint32_t number) {
    number_array.resize(NUMBER_LENGTH);
    *this = LongNumber();
    number_array[0] = number;
}

LongNumber::LongNumber(const std::string hex) {
    number_array.resize(NUMBER_LENGTH);
    *this = ConvertToNumber(hex);
}

LongNumber LongNumber::zero() {
    return LongNumber(0);
}

LongNumber LongNumber::one() {
    return LongNumber(1);
}

LongNumber LongNumber::operator+(LongNumber& B) const {
    LongNumber result;
    uint32_t carry = 0;
    for (int i = 0; i < NUMBER_LENGTH; i++) {
        uint64_t temp = static_cast<uint64_t>(this->number_array[i]) + B.number_array[i] + carry;
        result.number_array[i] = temp & UINT32_MAX;
        carry = temp >> 32;
    }
    return result;
}

LongNumber LongNumber::operator-(LongNumber& B) const {
    LongNumber result;
    uint32_t borrow = 0;
    for (int i = 0; i < NUMBER_LENGTH; i++) {
        int64_t temp = static_cast<int64_t>(this->number_array[i]) - static_cast<int64_t>(B.number_array[i]) - borrow;
        if (temp >= 0) {
            result.number_array[i] = static_cast<uint32_t>(temp);
            borrow = 0;
        }
        else {
            result.number_array[i] = static_cast<uint32_t>(UINT32_MAX + temp + 1);
            borrow = 1;
        }
    }
    if (borrow == 1) {
        throw std::invalid_argument("number \n" + ConvertToHex(*this) + "\nsmaller than \n" + ConvertToHex(B));
    }
    return result;
}

LongNumber LongNumber::operator*(uint32_t B) const {
    uint32_t carry = 0;
    LongNumber result;
    for (int i = 0; i < size(); i++) {
        uint64_t temp = static_cast<uint64_t>(this->number_array[i]) * static_cast<uint64_t>(B) + static_cast<uint64_t>(carry);
        result.number_array[i] = static_cast<uint32_t>(temp & UINT32_MAX);
        carry = static_cast<uint32_t>(temp >> 32);
    }
    result.number_array[size() - 1] = carry;
    return result;
}

LongNumber LongNumber::operator*(LongNumber& B) const {
    LongNumber result;
    for (int i = 0; i < NUMBER_LENGTH; i++) {
        LongNumber temp = *this * B.number_array[i];
        temp = temp.LongShiftDigitsToHigh(i);
        result = result + temp;
    }
    return result;
}


LongNumber LongNumber::operator/(LongNumber& B) const {
    int k = B.BitLength();
    LongNumber Q;
    LongNumber R(*this);
    while (R >= B) {
        int temp = R.BitLength();
        LongNumber C = B.LongShiftBitsToHigh(temp - k);
        if (R < C) {
            temp--;
            C = B.LongShiftBitsToHigh(temp - k);
        }
        R = R - C;
        LongNumber powerOfTwo(1);
        powerOfTwo = powerOfTwo.LongShiftBitsToHigh(temp - k);
        Q = Q + powerOfTwo;
    }
    return Q;
}

LongNumber LongNumber::operator%(LongNumber& B) const {
    int k = B.BitLength();
    LongNumber Q;
    LongNumber R(*this);
    while (R >= B) {
        int temp = R.BitLength();
        LongNumber C = B.LongShiftBitsToHigh(temp - k);
        if (R < C) {
            temp--;
            C = B.LongShiftBitsToHigh(temp - k);
        }
        R = R - C;
        LongNumber powerOfTwo(1);
        powerOfTwo = powerOfTwo.LongShiftBitsToHigh(temp - k);
        Q = Q + powerOfTwo;
    }
    return R;
}
LongNumber LongNumber::power(LongNumber& B) const {
    LongNumber result(1);
    for (int i = B.BitLength() - 1; i >= 0; i--) {
        uint32_t bit = B.number_array[i / 32] >> i % 32;
        bit = bit & 1;
        if (bit == 1) {
            result = *this * result;
        }
        if (i > 0) {
            result = result * result;
        }
    }
    return result;
}

int LongNumber::BitLength() const {
    int digit = NUMBER_LENGTH - 1;
    while (digit >= 0 && this->number_array[digit] == 0) {
        digit--;
    }
    if (digit == -1) return 0;
    int bit_length = digit * 32;
    uint32_t temp = this->number_array[digit];
    while (temp != 0) {
        temp >>= 1;
        bit_length++;
    }
    return bit_length;
}

int LongNumber::DigitLength() const {
    std::size_t count = BitLength();

    if (count % 32 > 0)
        return (count / 32) + 1;

    return count / 32;
}
bool LongNumber::operator<(LongNumber& B) const {
    int i = NUMBER_LENGTH - 1;
    while (i >= 0 && this->number_array[i] == B.number_array[i]) {
        i--;
    }
    if (i == -1) return false;
    return this->number_array[i] < B.number_array[i];
}

bool LongNumber::operator>(LongNumber& B) const {
    int i = NUMBER_LENGTH - 1;
    while (i >= 0 && this->number_array[i] == B.number_array[i]) {
        i--;
    }
    if (i == -1) return false;
    return this->number_array[i] > B.number_array[i];
}

bool LongNumber::operator<=(LongNumber& B) const {
    return *this < B || *this == B;
}

bool LongNumber::operator>=(LongNumber& B) const {
    return *this > B || *this == B;
}

bool LongNumber::operator==(LongNumber& const B) const {
    int i = NUMBER_LENGTH - 1;
    while (i >= 0 && this->number_array[i] == B.number_array[i]) {
        i--;
    }
    return i == -1;
}

bool LongNumber::operator!=(LongNumber& B) const {
    return !(*this == B);
}

LongNumber LongNumber::GCD(LongNumber& A, LongNumber& B) {
    LongNumber Q, R = 0;
    LongNumber aTemp = A;
    LongNumber bTemp = B;
    LongNumber Zero = zero();
    while (bTemp > Zero) {
        Q = aTemp / bTemp;
        LongNumber qTemp = Q * bTemp;
        R = aTemp - qTemp;
        aTemp = bTemp;
        bTemp = R;
    }
    return aTemp;
}

LongNumber LongNumber::LCM(LongNumber& A, LongNumber& B) {
    LongNumber a = (A * B);
    LongNumber b = GCD(A, B);
    return a / b;
}

LongNumber LongNumber::LongShiftDigitsToLow(int shift) const {
    LongNumber result;
    for (int i = NUMBER_LENGTH - 1; i >= shift; i--) {
        result.number_array[i - shift] = this->number_array[i];
    }
    return result;
}

LongNumber LongNumber::LongShiftDigitsToHigh(int shift) const {
    LongNumber result = *this;
    for (int i = 0; i < NUMBER_LENGTH - shift; i++) {
        result.number_array[i + shift] = this->number_array[i];
    }
    return result;
}

LongNumber LongNumber::LongShiftBitsToHigh(int shift) const {
    int digitShifts = shift / 32;
    int bitShifts = shift % 32;
    uint32_t carry = 0;
    LongNumber temp(*this);
    LongNumber result;
    if (bitShifts == 0) {
        for (int i = 0; i < NUMBER_LENGTH; i++) {
            temp.number_array[i] = this->number_array[i];
        }
        for (int j = digitShifts; j < NUMBER_LENGTH; j++) {
            result.number_array[j] = temp.number_array[j - digitShifts];
        }
    }
    else {
        for (int i = 0; i < NUMBER_LENGTH; i++) {
            temp.number_array[i] = (this->number_array[i] << shift) + carry;
            carry = (this->number_array[i] >> (32 - shift));
        }
        for (int j = digitShifts; j < NUMBER_LENGTH; j++) {
            result.number_array[j] = temp.number_array[j - digitShifts];
        }
    }
    return result;
}
LongNumber LongNumber::LongShiftBitsToLow(int shift) const {
    int digitShifts = shift / 32;
    int bitShifts = shift % 32;
    LongNumber result = (*this);

    // зсув на цілі слова
    if (digitShifts >= static_cast<int>(result.size())) {
        result.number_array.clear();
        result.number_array.push_back(0);
        return result;
    }

    for (int i = 0; i < static_cast<int>(result.size()) - digitShifts; ++i) {
        result.number_array[i] = result.number_array[i + digitShifts];
    }
    for (int i = static_cast<int>(result.size()) - digitShifts; i < static_cast<int>(result.size()); ++i) {
        result.number_array[i] = 0;
    }

    //побітовий зсув з переносом
    if (bitShifts > 0) {
        uint32_t carry = 0;
        for (int i = static_cast<int>(result.size()) - 1; i >= 0; --i) {
            uint32_t current = result.number_array[i];
            result.number_array[i] = (current << bitShifts) | carry;
            carry = current >> (32 - bitShifts);
        }
    }

    result.normalize();
    return result;
}

char ConvertDigitToHex(uint32_t digit) {
    std::string hexs = "0123456789abcdef";
    return hexs[digit];
}

uint32_t ConvertHexToDigit(char hex) {
    if (hex == '0') return 0;
    if (hex == '1') return 1;
    if (hex == '2') return 2;
    if (hex == '3') return 3;
    if (hex == '4') return 4;
    if (hex == '5') return 5;
    if (hex == '6') return 6;
    if (hex == '7') return 7;
    if (hex == '8') return 8;
    if (hex == '9') return 9;
    if (hex == 'A' || hex == 'a') return 10;
    if (hex == 'B' || hex == 'b') return 11;
    if (hex == 'C' || hex == 'c') return 12;
    if (hex == 'D' || hex == 'd') return 13;
    if (hex == 'E' || hex == 'e') return 14;
    if (hex == 'F' || hex == 'f') return 15;

    throw std::invalid_argument("Digit is incorrect: " + hex);
}

LongNumber ConvertToNumber(std::string hex) {
    LongNumber result;
    for (int i = 0; i < hex.length(); i++) {
        uint32_t modulo = i % 8;
        uint32_t digit = ConvertHexToDigit(hex[hex.length() - i - 1]);
        result.number_array[i / 8] += digit * (1 << (4 * modulo));
    }
    return result;
}

std::string ConvertToHex(LongNumber number) {
    std::string hex = "";
    for (int i = 0; i < NUMBER_LENGTH * 8; i++) {
        uint32_t modulo = i % 8;
        hex += ConvertDigitToHex((number.number_array[i / 8] >> (4 * modulo)) & 15);
    }
    hex = hex.erase(hex.find_last_not_of('0') + 1);
    std::string reverse_hex;
    for (int i = hex.length() - 1; i >= 0; i--) {
        reverse_hex += hex[i];
    }
    return reverse_hex;
}

std::ostream& operator<<(std::ostream& os, const LongNumber& number) {
    os << ConvertToHex(number);
    return os;
}


LongNumberModulo::LongNumberModulo(LongNumber& mod) {
    modulo = mod;
    mu = Mu(mod);
    digitLength = modulo.DigitLength();
}

LongNumber LongNumberModulo::add(LongNumber& A, LongNumber& B) {
    LongNumber result = A + B;
    return Barrett(result);
}

LongNumber LongNumberModulo::sub(LongNumber& A, LongNumber& B) {
    LongNumber result;
    if (A < B) {
        result = B - A;
        result = Barrett(result);
        result = modulo - result;
    }
    else {
        result = A - B;
        result = Barrett(result);
    }
    return result;
}

LongNumber LongNumberModulo::mult(LongNumber& A, uint32_t& B) {
    LongNumber result = A * B;
    return Barrett(result);
}

LongNumber LongNumberModulo::mult(LongNumber& A, LongNumber& B) {
    LongNumber result = A * B;
    return Barrett(result);
}

LongNumber LongNumberModulo::square(LongNumber& A) {
    LongNumber result = A * A;
    return Barrett(result);
}

LongNumber LongNumberModulo::power(LongNumber& A, LongNumber& power) {
    LongNumber result = LongNumber::one();
    LongNumber base = A;

    for (int i = 0; i < NUMBER_LENGTH; i++) {
        uint32_t word = power.number_array[i];
        for (int j = 0; j < 32; j++) {
            if (word & 1) {
                result = mult(result, base);
            }
            base = mult(base, base);
            word >>= 1;
        }
    }
    return result;
}

//LongNumber LongNumberModulo::Barrett(LongNumber& A) {
//    if (A < modulo) return A;
//    return A % modulo;
//}

//LongNumber LongNumberModulo::Barrett(LongNumber& A) {
//    if (A < modulo) return A;
//
//    LongNumber Q = A.LongShiftDigitsToLow(digitLength - 1);
//    Q = Q * mu;
//    Q = Q.LongShiftDigitsToLow(digitLength + 1);
//    Q = Q * modulo;
//
//    LongNumber R = A - Q;
//
//    while (R >= modulo) {
//        R = R - modulo;
//    }
//    
//    //if (R >= modulo) R = R - modulo;
//    //if (R >= modulo) R = R - modulo;
//
//    return R;
//}

LongNumber LongNumberModulo::Barrett(LongNumber& A)
{
    std::size_t k = modulo.DigitLength();
    LongNumber q = A.LongShiftBitsToLow((k - 1) * 32);
    q = q * mu;
    q = q.LongShiftBitsToLow((k + 1) * 32);
    LongNumber temp = q * modulo;
    LongNumber reduction = A - temp;
    while (reduction >= modulo)
    {
        try {
            reduction = reduction - modulo;
        }
        catch (std::invalid_argument a) {
            std::cout << a.what();
        }

    }


    return reduction;
}


LongNumber LongNumberModulo::Mu(LongNumber& mod) {
    int digitLength = mod.DigitLength();
    LongNumber beta(1);
    beta = beta.LongShiftBitsToHigh(digitLength * 2 * 32);
    return beta / mod;
}

LongNumber generateLongNumber(int n) {
    std::string hexs = "0123456789abcdef";
    std::string result = "";
    result += hexs[rand() % 15 + 1];
    for (int i = 1; i < n; i++) {
        result += hexs[rand() % 16];
    }
    return LongNumber(result);
}
void TestRawOperations() {
    LongNumber A("80307b356ca65d93e9aba1a011901a8f858da76cac9995519cc5287887fdfa8b114da61f5b3f7e0bf02a3a96d3d3711f6b8f3f7ee4387fcda20d52ddacc4cfa72cd98b6b7360d037a7c8325cc25e7450ed4eb4e7c3d75a567f982584a438028b20397fc3f0c66e6682559d669fb22d12ca72c82965701370ee6c9e556c897569");
    LongNumber B("47293d9cf232ebcbf2bfafed458903bbc8f02f9d3083bb5dc405cc0f659e833fa90d597bc79c4396fad89e16f655410de3c54caba4b2a96cf6555a05b5b6d93018b2636169522b048ebe7563e42ee2307700c87ab2935dab71431a70e34f96f46abba42cb7cf8ab6c97f1d636b2216641063c305291639a377dcc0e4af8f5e5d");

    std::cout << "\nLab 1\n";

    long long nanoseconds = 0;
    auto start = chrono::high_resolution_clock::now();
    auto result = A + B;
    auto end = chrono::high_resolution_clock::now();
    nanoseconds = chrono::duration_cast<chrono::nanoseconds>(end - start).count();
    cout << "A + B = " << result << '\n';
    cout << "Elapsed nanoseconds for addition: " << nanoseconds << '\n';

    try {
        start = chrono::high_resolution_clock::now();
        result = A - B;
        end = chrono::high_resolution_clock::now();
        nanoseconds = chrono::duration_cast<chrono::nanoseconds>(end - start).count();
        cout << "A - B = " << result << '\n';
        cout << "Elapsed nanoseconds for subtraction: " << nanoseconds << '\n';
    }
    catch (const std::invalid_argument& e) {
        cout << "A - B = [ERROR]: " << e.what() << '\n';
    }

    start = chrono::high_resolution_clock::now();
    result = A * B;
    end = chrono::high_resolution_clock::now();
    nanoseconds = chrono::duration_cast<chrono::nanoseconds>(end - start).count();
    cout << "A * B = " << result << '\n';
    cout << "Elapsed nanoseconds for multiplication: " << nanoseconds << '\n';

    start = chrono::high_resolution_clock::now();
    result = A / B;
    end = chrono::high_resolution_clock::now();
    nanoseconds = chrono::duration_cast<chrono::nanoseconds>(end - start).count();
    cout << "A / B = " << result << '\n';
    cout << "Elapsed nanoseconds for division: " << nanoseconds << '\n';

    start = chrono::high_resolution_clock::now();
    result = A % B;
    end = chrono::high_resolution_clock::now();
    nanoseconds = chrono::duration_cast<chrono::nanoseconds>(end - start).count();
    cout << "A % B = " << result << '\n';
    cout << "Elapsed nanoseconds for modulo: " << nanoseconds << '\n';

    start = chrono::high_resolution_clock::now();
    result = A.power(B);
    end = chrono::high_resolution_clock::now();
    nanoseconds = chrono::duration_cast<chrono::nanoseconds>(end - start).count();
    cout << "A ^ B = " << result << '\n';
    cout << "Elapsed nanoseconds for power: " << nanoseconds << '\n';

    /*int shiftAmount = 2;
    start = chrono::high_resolution_clock::now();
    result = A.LongShiftBitsToLow(shiftAmount);
    end = chrono::high_resolution_clock::now();
    nanoseconds = chrono::duration_cast<chrono::nanoseconds>(end - start).count();
    cout << "Bit shift right by " << shiftAmount << " bits (A >> " << shiftAmount << ") =\n" << result << '\n';
    cout << "Elapsed nanoseconds for bit shift: " << nanoseconds << '\n';*/
}

void TestNumbers() {
    LongNumber A("80307b356ca65d93e9aba1a011901a8f858da76cac9995519cc5287887fdfa8b114da61f5b3f7e0bf02a3a96d3d3711f6b8f3f7ee4387fcda20d52ddacc4cfa72cd98b6b7360d037a7c8325cc25e7450ed4eb4e7c3d75a567f982584a438028b20397fc3f0c66e6682559d669fb22d12ca72c82965701370ee6c9e556c897569");
    LongNumber B("47293d9cf232ebcbf2bfafed458903bbc8f02f9d3083bb5dc405cc0f659e833fa90d597bc79c4396fad89e16f655410de3c54caba4b2a96cf6555a05b5b6d93018b2636169522b048ebe7563e42ee2307700c87ab2935dab71431a70e34f96f46abba42cb7cf8ab6c97f1d636b2216641063c305291639a377dcc0e4af8f5e5d");
    LongNumber N("a4519ca8fba424765f1b795e4ddbbc89e022c84ded4523d54a7fb881097305a196fc85b6f4f765a97d3ff30af17824a6d6dc2a91d181065df345ceb72144ea7d310563c5b72c155850617102b3e1179b942dfc05303e314c8649698c26102f4bab2b1444a08d215e60c561ff47bc14da6a5895bbdd3b473575afadbb15d962f5");
    LongNumberModulo modulo(N);



    std::cout << "\nLab 2\n";

    auto start = chrono::high_resolution_clock::now();
    auto result = modulo.add(A, B);
    auto end = chrono::high_resolution_clock::now();
    cout << "A + B mod N = " << result << '\n';
    cout << "Elapsed nanoseconds for addition: " << chrono::duration_cast<chrono::nanoseconds>(end - start).count() << '\n';

    start = chrono::high_resolution_clock::now();
    result = modulo.sub(A, B);
    end = chrono::high_resolution_clock::now();
    cout << "A - B mod N = " << result << '\n';
    cout << "Elapsed nanoseconds for subtraction: " << chrono::duration_cast<chrono::nanoseconds>(end - start).count() << '\n';

    start = chrono::high_resolution_clock::now();
    result = LongNumber::GCD(A, B);
    end = chrono::high_resolution_clock::now();
    cout << "gcd(A,B) = " << result << '\n';
    cout << "Elapsed nanoseconds for gcd: " << chrono::duration_cast<chrono::nanoseconds>(end - start).count() << '\n';

    start = chrono::high_resolution_clock::now();
    result = LongNumber::LCM(A, B);
    end = chrono::high_resolution_clock::now();
    cout << "lcm(A,B) = " << result << '\n';
    cout << "Elapsed nanoseconds for lcm: " << chrono::duration_cast<chrono::nanoseconds>(end - start).count() << '\n';

    start = chrono::high_resolution_clock::now();
    result = modulo.mult(A, B);
    end = chrono::high_resolution_clock::now();
    cout << "A * B mod N = " << result << '\n';
    cout << "Elapsed nanoseconds for multiplication: " << chrono::duration_cast<chrono::nanoseconds>(end - start).count() << '\n';

    start = chrono::high_resolution_clock::now();
    result = modulo.square(A);
    end = chrono::high_resolution_clock::now();
    cout << "A ^ 2 mod N = " << result << '\n';
    cout << "Elapsed nanoseconds for square: " << chrono::duration_cast<chrono::nanoseconds>(end - start).count() << '\n';

    start = chrono::high_resolution_clock::now();
    result = modulo.power(A, B);
    end = chrono::high_resolution_clock::now();
    cout << "A ^ B mod N = " << result << '\n';
    cout << "Elapsed nanoseconds for power: " << chrono::duration_cast<chrono::nanoseconds>(end - start).count() << '\n';


}




int main() {
    TestRawOperations();
    TestNumbers();

    int numberOfMeasurements = 200;
    int size = 128;
    long long nanoseconds = 0;

    for (int i = 0; i < numberOfMeasurements; i++) {
        LongNumber first = generateLongNumber(size);
        LongNumber second = generateLongNumber(size);
        LongNumber mod = generateLongNumber(size / 2);
        LongNumberModulo modulo(mod);

        const auto start{ std::chrono::high_resolution_clock::now() };
        modulo.add(first, second);
        const auto end{ std::chrono::high_resolution_clock::now() };
        nanoseconds += std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count();
    }
    std::cout << "Elapsed nanoseconds for addition: " << nanoseconds / numberOfMeasurements << '\n';

    nanoseconds = 0;
    for (int i = 0; i < numberOfMeasurements; i++) {
        LongNumber first = generateLongNumber(size);
        LongNumber second = generateLongNumber(size);
        LongNumber mod = generateLongNumber(size / 2);
        LongNumberModulo modulo(mod);

        const auto start{ std::chrono::high_resolution_clock::now() };
        modulo.sub(first, second);
        const auto end{ std::chrono::high_resolution_clock::now() };
        nanoseconds += std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count();
    }
    std::cout << "Elapsed nanoseconds for subtraction: " << nanoseconds / numberOfMeasurements << '\n';

    nanoseconds = 0;
    for (int i = 0; i < numberOfMeasurements; i++) {
        LongNumber first = generateLongNumber(size);
        LongNumber second = generateLongNumber(size);
        LongNumber mod = generateLongNumber(size);
        LongNumberModulo modulo(mod);

        const auto start{ std::chrono::high_resolution_clock::now() };
        modulo.mult(first, second);
        const auto end{ std::chrono::high_resolution_clock::now() };
        nanoseconds += std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count();
    }
    std::cout << "Elapsed nanoseconds for multiplication: " << nanoseconds / numberOfMeasurements << '\n';

    nanoseconds = 0;
    for (int i = 0; i < numberOfMeasurements; i++) {
        LongNumber first = generateLongNumber(size);
        LongNumber second = generateLongNumber(size / 2);
        LongNumber mod = generateLongNumber(size);
        LongNumberModulo modulo(mod);

        const auto start{ std::chrono::high_resolution_clock::now() };
        modulo.power(first, second);
        const auto end{ std::chrono::high_resolution_clock::now() };
        nanoseconds += std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count();
    }
    std::cout << "Elapsed nanoseconds for power: " << nanoseconds / numberOfMeasurements << '\n';

    nanoseconds = 0;
    for (int i = 0; i < numberOfMeasurements; i++) {
        auto first = generateLongNumber(size);
        auto second = generateLongNumber(size);

        const auto start{ std::chrono::high_resolution_clock::now() };
        LongNumber::GCD(first, second);
        const auto end{ std::chrono::high_resolution_clock::now() };
        nanoseconds += std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count();
    }
    std::cout << "Elapsed nanoseconds for gcd: " << nanoseconds / numberOfMeasurements << '\n';

    nanoseconds = 0;
    for (int i = 0; i < numberOfMeasurements; i++) {
        auto first = generateLongNumber(size);
        auto second = generateLongNumber(size);

        const auto start{ std::chrono::high_resolution_clock::now() };
        LongNumber::LCM(first, second);
        const auto end{ std::chrono::high_resolution_clock::now() };
        nanoseconds += std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count();
    }
    std::cout << "Elapsed nanoseconds for lcm: " << nanoseconds / numberOfMeasurements << '\n';
}