# Iodine

**Iodine** is a lightweight and expressive scripting language designed with simplicity, extensibility, and performance in mind. Built using **OCaml**.

---

## **Features**

- **Elegant Syntax:** A concise and readable syntax that encourages productivity and clarity.
  
- **OCaml-Powered:** Built on the robust OCaml ecosystem, ensuring reliability and efficiency.

- **Lightweight Design:** Minimal dependencies for quick setup and streamlined development.

- **Highly Extensible:** Adapt Iodine to suit your specific project needs with ease.

---

## **Getting Started**

### **Prerequisites**

Ensure the following tools are installed on your system before proceeding:

- **OCaml** (version 5.2.1 or higher)
- **Dune** (OCaml's build system)
- **ppx_deriving** (a tool for code generation)

---

### **Installation**

Follow these steps to set up and build Iodine on your machine:

#### 1. **Install OCaml**
- **Windows:**
  ```bash
  winget install Git.Git OCaml.opam
  ```
- **macOS/Linux:**
  ```bash
  bash -c "sh <(curl -fsSL https://opam.ocaml.org/install.sh)"
  ```

#### 2. **Initialize Opam**
```bash
opam init
```

#### 3. **Activate the Opam Environment**
- **Windows:**
  ```bash
  (& opam env) -split '\r?\n' | ForEach-Object { Invoke-Expression $_ }
  ```
- **macOS/Linux:**
  ```bash
  eval $(opam env)
  ```

#### 4. **Install Dependencies**
```bash
opam install dune ppx_deriving
```

#### 5. **Clone the Repository**
```bash
git clone https://github.com/PeterGriffinSr/Iodine
cd Iodine
```

#### 6. **Build the Project**
```bash
dune build
```

---

## **Contributing**

We welcome contributions to Iodine! Feel free to submit issues, suggest features, or create pull requests to improve the language.

---