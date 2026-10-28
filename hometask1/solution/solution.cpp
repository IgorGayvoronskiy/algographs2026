#include <iostream>
#include <vector>
#include <queue>
#include <sstream>
#include <string>
#include <csignal>
#include <cstdint>
#include <random>
#include <algorithm>

using namespace std;

volatile sig_atomic_t stop = 0;

void on_sigterm(int signum) {
    stop = 1;
}

int n;
vector<vector<int>> adj, radj;
vector<char> alive, in_x;
vector<int> in_deg, out_deg;
vector<int> comp;
vector<double> noise;

void find_scc() {
    comp.assign(n + 1, -1);
    vector<char> visited(n + 1, 0);
    vector<int> pos(n + 1, 0);
    vector<int> order;
    vector<int> call;
    vector<int> st;
    vector<int> comp_size;
    int counter = 0;

    for (int s = 1; s <= n; ++s) {
        if (!alive[s] || visited[s]) {
            continue;
        }
        visited[s] = 1;
        call.push_back(s);

        while (!call.empty()) {
            int v = call.back();
            if (pos[v] < (int)adj[v].size()) {
                int u = adj[v][pos[v]++];
                if (!alive[u] || visited[u]) {
                    continue;
                }
                visited[u] = 1;
                call.push_back(u);
            }
            else {
                order.push_back(v);
                call.pop_back();
            }
        }
    }

    for (int i = (int)order.size() - 1; i >= 0; --i) {
        int s = order[i];
        if (comp[s] != -1) {
            continue;
        }
        int size = 1;
        comp[s] = counter;
        st.push_back(s);

        while (!st.empty()) {
            int v = st.back();
            st.pop_back();
            for (int u : radj[v]) {
                if (!alive[u] || comp[u] != -1) {
                    continue;
                }
                comp[u] = counter;
                ++size;
                st.push_back(u);
            }
        }
        comp_size.push_back(size);
        ++counter;
    }

    for (int v = 1; v <= n; ++v) {
        if (alive[v] && comp_size[comp[v]] == 1) {
            alive[v] = false;
        }
    }
}

void calc_degrees() {
    in_deg.assign(n + 1, 0);
    out_deg.assign(n + 1, 0);
    for (int v = 1; v <= n; ++v) {
        if (!alive[v]) {
            continue;
        }
        for (int u : adj[v]) {
            if (alive[u] && comp[u] == comp[v]) {
                ++out_deg[v];
                ++in_deg[u];
            }
        }
    }
}

void remove_vertex(int v) {
    vector<int> q;
    alive[v] = false;
    q.push_back(v);
    while (!q.empty()) {
        int x = q.back();
        q.pop_back();
        for (int u : adj[x]) {
            if (alive[u] && comp[u] == comp[x]) {
                --in_deg[u];
                if (in_deg[u] == 0) {
                    alive[u] = false;
                    q.push_back(u);
                }
            }
        }
        for (int u : radj[x]) {
            if (alive[u] && comp[u] == comp[x]) {
                --out_deg[u];
                if (out_deg[u] == 0) {
                    alive[u] = false;
                    q.push_back(u);
                }
            }
        }
    }
}

double key(int v) {
    return (double)in_deg[v] * out_deg[v] * noise[v];
}

bool greedy(vector<int>& result, bool can_abort) {
    result.clear();
    while (true) {
        if (can_abort && stop) {
            return false;
        }
        find_scc();
        calc_degrees();

        int counter = 0;
        priority_queue<pair<double, int>> pq;
        for (int v = 1; v <= n; ++v) {
            if (alive[v]) {
                ++counter;
                pq.push({key(v), v});
            }
        }
        if (counter == 0) {
            break;
        }

        int batch = counter <= 2000 ? 1 : max(1, counter / 200);
        int removed = 0;
        while (removed < batch && !pq.empty()) {
            pair<double, int> top = pq.top();
            pq.pop();
            int v = top.second;
            if (!alive[v]) {
                continue;
            }
            double cur = key(v);
            if (cur != top.first) {
                pq.push({cur, v});
                continue;
            }
            result.push_back(v);
            in_x[v] = true;
            remove_vertex(v);
            ++removed;
        }
    }
    return true;
}

vector<int> step_visited;
int step = 0;

bool is_on_cycle(int x) {
    ++step;
    vector<int> q;
    for (int u : adj[x]) {
        if (in_x[u]) {
            continue;
        }
        if (u == x) {
            return true;
        }
        if (step_visited[u] != step) {
            step_visited[u] = step;
            q.push_back(u);
        }
    }
    while (!q.empty()) {
        int v = q.back();
        q.pop_back();
        for (int u : adj[v]) {
            if (in_x[u]) {
                continue;
            }
            if (u == x) {
                return true;
            }
            if (step_visited[u] != step) {
                step_visited[u] = step;
                q.push_back(u);
            }
        }
    }
    return false;
}

void cleanup(const vector<int>& x_list) {
    step_visited.assign(n + 1, 0);
    for (int i = (int)x_list.size() - 1; i >= 0; --i) {
        if (stop) {
            break;
        }
        int v = x_list[i];
        in_x[v] = false;
        if (is_on_cycle(v)) {
            in_x[v] = true;
        }
    }
}

int count_x() {
    int count = 0;
    for (int v = 1; v <= n; ++v) {
        if (in_x[v]) {
            ++count;
        }
    }
    return count;
}

int main(int argc, char* argv[])
{
    signal(SIGTERM, on_sigterm);

    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int m, zero;
    cin >> n >> m >> zero;
    string line;
    getline(cin, line);

    adj.resize(n + 1);
    radj.resize(n + 1);
    for (int i = 1; i <= n; ++i) {
        getline(cin, line);
        istringstream ss(line);
        int v;
        while (ss >> v) {
            adj[i].push_back(v);
            radj[v].push_back(i);
        }
    }

    mt19937 rng(712367);

    alive.assign(n + 1, 1);
    in_x.assign(n + 1, 0);
    noise.assign(n + 1, 1.0);

    vector<int> x_list;
    greedy(x_list, false);
    cleanup(x_list);

    vector<char> best_x = in_x;
    int best_size = count_x();

    long long restarts = min(3000LL, 400000000LL / ((long long)n + m + 1));
    for (long long r = 1; r <= restarts && !stop; ++r) {
        alive.assign(n + 1, 1);
        in_x.assign(n + 1, 0);
        for (int v = 1; v <= n; ++v) {
            noise[v] = 1.0 + 0.5 * (rng() % 1000) / 1000.0;
        }
        if (!greedy(x_list, true)) {
            break;
        }
        shuffle(x_list.begin(), x_list.end(), rng);
        cleanup(x_list);

        int size = count_x();
        if (size < best_size) {
            best_size = size;
            best_x = in_x;
        }
    }

    for (int v = 1; v <= n; ++v) {
        if (best_x[v]) {
            cout << v << "\n";
        }
    }
}