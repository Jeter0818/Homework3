#include <iostream>
#include <cmath>
using namespace std;

template <class T>
class Chain;

template <class T>
class ChainIterator;

class Polynomial;

template <class T>
class ChainNode {
    friend class Chain<T>;
    friend class ChainIterator<T>;
    friend class ::Polynomial;
private:
    T data;
    ChainNode<T>* link;
public:
    ChainNode(const T& element = T(), ChainNode<T>* next = nullptr)
        : data(element), link(next) {}
};

template <class T>
class Chain {
    friend class ChainIterator<T>;
protected:
    ChainNode<T>* header;
    static ChainNode<T>* av;

public:
    typedef ChainIterator<T> iterator;

    Chain() {
        header = new ChainNode<T>();
        header->link = header;
    }

    static ChainNode<T>* GetNode(const T& val = T(), ChainNode<T>* next = nullptr) {
        if (av != nullptr) {
            ChainNode<T>* node = av;
            av = av->link;
            node->data = val;
            node->link = next;
            return node;
        }
        return new ChainNode<T>(val, next);
    }

    static void RetNode(ChainNode<T>*& x) {
        if (x == nullptr) return;
        x->link = av;
        av = x;
        x = nullptr;
    }

    void ClearToAvail() {
        if (header == nullptr) return;
        ChainNode<T>* curr = header->link;
        while (curr != header) {
            ChainNode<T>* nextNode = curr->link;
            RetNode(curr);
            curr = nextNode;
        }
        header->link = header;
    }

    void EraseAllToAvail() {
        if (header == nullptr) return;
        ClearToAvail();
        RetNode(header);
    }

    static void FreeAvailMemory() {
        while (av != nullptr) {
            ChainNode<T>* temp = av;
            av = av->link;
            delete temp;
        }
    }

    bool IsEmpty() const {
        return header->link == header;
    }

    ChainNode<T>* GetHeader() const {
        return header;
    }

    void InsertBack(const T& val) {
        ChainNode<T>* last = header;
        while (last->link != header) {
            last = last->link;
        }
        ChainNode<T>* newNode = GetNode(val, header);
        last->link = newNode;
    }

    iterator Begin() const {
        return iterator(header->link);
    }

    iterator End() const {
        return iterator(header);
    }
};

template <class T>
ChainNode<T>* Chain<T>::av = nullptr;

template <class T>
class ChainIterator {
private:
    ChainNode<T>* current;
public:
    ChainIterator(ChainNode<T>* startNode = nullptr) : current(startNode) {}

    T& operator*() const { return current->data; }
    T* operator->() const { return &current->data; }

    ChainIterator& operator++() {
        current = current->link;
        return *this;
    }

    ChainIterator operator++(int) {
        ChainIterator old = *this;
        current = current->link;
        return old;
    }

    bool operator==(const ChainIterator& right) const {
        return current == right.current;
    }

    bool operator!=(const ChainIterator& right) const {
        return current != right.current;
    }

    ChainNode<T>* GetNodePtr() const {
        return current;
    }
};

struct Term {
    int coef;
    int exp;
    Term(int c = 0, int e = 0) : coef(c), exp(e) {}
};

class Polynomial {
private:
    Chain<Term> poly;

    void AddOrInsertTerm(int c, int e) {
        if (c == 0) return;

        ChainNode<Term>* head = poly.GetHeader();
        ChainNode<Term>* prev = head;
        ChainNode<Term>* curr = head->link;

        while (curr != head && curr->data.exp > e) {
            prev = curr;
            curr = curr->link;
        }

        if (curr != head && curr->data.exp == e) {
            curr->data.coef += c;
            if (curr->data.coef == 0) {
                prev->link = curr->link;
                Chain<Term>::RetNode(curr);
            }
        } else {
            ChainNode<Term>* newNode = Chain<Term>::GetNode(Term(c, e), curr);
            prev->link = newNode;
        }
    }

public:
    Polynomial() {}

    Polynomial(const Polynomial& a) {
        for (Chain<Term>::iterator it = a.poly.Begin(); it != a.poly.End(); ++it) {
            poly.InsertBack(*it);
        }
    }

    const Polynomial& operator=(const Polynomial& a) {
        if (this != &a) {
            poly.ClearToAvail();
            for (Chain<Term>::iterator it = a.poly.Begin(); it != a.poly.End(); ++it) {
                poly.InsertBack(*it);
            }
        }
        return *this;
    }

    ~Polynomial() {
        poly.EraseAllToAvail();
    }

    Polynomial operator+(const Polynomial& b) const {
        Polynomial result;
        Chain<Term>::iterator itA = this->poly.Begin();
        Chain<Term>::iterator itB = b.poly.Begin();

        while (itA != this->poly.End() && itB != b.poly.End()) {
            if (itA->exp > itB->exp) {
                result.poly.InsertBack(*itA);
                ++itA;
            } else if (itA->exp < itB->exp) {
                result.poly.InsertBack(*itB);
                ++itB;
            } else {
                int sumCoef = itA->coef + itB->coef;
                if (sumCoef != 0) {
                    result.poly.InsertBack(Term(sumCoef, itA->exp));
                }
                ++itA;
                ++itB;
            }
        }

        while (itA != this->poly.End()) {
            result.poly.InsertBack(*itA);
            ++itA;
        }

        while (itB != b.poly.End()) {
            result.poly.InsertBack(*itB);
            ++itB;
        }

        return result;
    }

    Polynomial operator-(const Polynomial& b) const {
        Polynomial result;
        Chain<Term>::iterator itA = this->poly.Begin();
        Chain<Term>::iterator itB = b.poly.Begin();

        while (itA != this->poly.End() && itB != b.poly.End()) {
            if (itA->exp > itB->exp) {
                result.poly.InsertBack(*itA);
                ++itA;
            } else if (itA->exp < itB->exp) {
                result.poly.InsertBack(Term(-(itB->coef), itB->exp));
                ++itB;
            } else {
                int diffCoef = itA->coef - itB->coef;
                if (diffCoef != 0) {
                    result.poly.InsertBack(Term(diffCoef, itA->exp));
                }
                ++itA;
                ++itB;
            }
        }

        while (itA != this->poly.End()) {
            result.poly.InsertBack(*itA);
            ++itA;
        }

        while (itB != b.poly.End()) {
            result.poly.InsertBack(Term(-(itB->coef), itB->exp));
            ++itB;
        }

        return result;
    }

    Polynomial operator*(const Polynomial& b) const {
        Polynomial result;
        for (Chain<Term>::iterator itA = this->poly.Begin(); itA != this->poly.End(); ++itA) {
            for (Chain<Term>::iterator itB = b.poly.Begin(); itB != b.poly.End(); ++itB) {
                int prodCoef = itA->coef * itB->coef;
                int sumExp = itA->exp + itB->exp;
                result.AddOrInsertTerm(prodCoef, sumExp);
            }
        }
        return result;
    }

    float Evaluate(float x) const {
        float total = 0.0f;
        for (Chain<Term>::iterator it = this->poly.Begin(); it != this->poly.End(); ++it) {
            total += static_cast<float>(it->coef) * pow(x, it->exp);
        }
        return total;
    }

    friend istream& operator>>(istream& is, Polynomial& x) {
        x.poly.ClearToAvail();
        int n;
        if (!(is >> n)) return is;

        for (int i = 0; i < n; ++i) {
            int c, e;
            is >> c >> e;
            if (c != 0) {
                x.poly.InsertBack(Term(c, e));
            }
        }
        return is;
    }

    friend ostream& operator<<(ostream& os, Polynomial& x) {
        int count = 0;
        for (Chain<Term>::iterator it = x.poly.Begin(); it != x.poly.End(); ++it) {
            if (it->coef != 0) {
                count++;
            }
        }

        os << count;
        for (Chain<Term>::iterator it = x.poly.Begin(); it != x.poly.End(); ++it) {
            if (it->coef != 0) {
                os << " " << it->coef << " " << it->exp;
            }
        }
        return os;
    }
};

int main() {
    Polynomial p1, p2;

    cout << "請輸入多項式 p1 (格式: n c1 e1 c2 e2 ...): ";
    cin >> p1;

    cout << "請輸入多項式 p2 (格式: n c1 e1 c2 e2 ...): ";
    cin >> p2;

    cout << "\np1: " << p1 << "\n";
    cout << "p2: " << p2 << "\n";

    Polynomial pCopy(p1);
    cout << "Copy of p1: " << pCopy << "\n";

    Polynomial pAssign;
    pAssign = p2;
    cout << "Assigned p2: " << pAssign << "\n";

    Polynomial sum = p1 + p2;
    cout << "p1 + p2: " << sum << "\n";

    Polynomial diff = p1 - p2;
    cout << "p1 - p2: " << diff << "\n";

    Polynomial prod = p1 * p2;
    cout << "p1 * p2: " << prod << "\n";

    float xVal;
    cout << "\n請輸入 x 值以計算 p1(x): ";
    cin >> xVal;
    cout << "p1(" << xVal << ") = " << p1.Evaluate(xVal) << "\n";

    Chain<Term>::FreeAvailMemory();
    return 0;
}