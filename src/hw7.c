#include "hw7.h"

bst_sf *insert_bst_sf(matrix_sf *mat, bst_sf *root)
{
    bst_sf *nodePointer;
    if (mat == NULL)
    {
        return root;
    }
    if (root == NULL)
    {
        nodePointer = malloc(sizeof(bst_sf));
        if (nodePointer == NULL)
        {
            return NULL;
        }

        (*nodePointer).mat = mat;
        (*nodePointer).left_child = NULL;

        (*nodePointer).right_child = NULL;

        return nodePointer;
    }
    if ((*mat).name == (*(*root).mat).name)
    {

        free((*root).mat);
        (*root).mat = mat;
    }
    else if ((*mat).name < (*(*root).mat).name)
    {
        (*root).left_child = insert_bst_sf(mat, (*root).left_child);
    }
    else
    {

        (*root).right_child = insert_bst_sf(mat, (*root).right_child);
    }

    return root;
}

matrix_sf *find_bst_sf(char name, bst_sf *root)
{
    if (root == NULL)
    {
        return NULL;
    }

    if (name == (*(*root).mat).name)
    {
        return (*root).mat;
    }
    else if (name < (*(*root).mat).name)
    {
        return find_bst_sf(name, (*root).left_child);
    }
    else
    {

        return find_bst_sf(name, (*root).right_child);
    }
}

void free_bst_sf(bst_sf *root)
{
    if (root == NULL)
    {
        return;
    }

    free_bst_sf((*root).left_child);

    free_bst_sf((*root).right_child);

    free((*root).mat);
    free(root);
}

matrix_sf *add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2)
{

    matrix_sf *res;
    int r1, c1, r2, c2;
    int i;

    if (mat1 == NULL || mat2 == NULL)
        return NULL;

    r1 = (*mat1).num_rows;

    c1 = (*mat1).num_cols;
    r2 = (*mat2).num_rows;

    c2 = (*mat2).num_cols;

    if (r1 != r2 || c1 != c2)
        return NULL;

    res = malloc(sizeof(matrix_sf) + r1 * c1 * sizeof(int));
    if (res == NULL)
        return NULL;

    (*res).name = '#';
    (*res).num_rows = r1;
    (*res).num_cols = c1;

    for (i = 0; i < r1 * c1; i++)
    {
        (*res).values[i] = (*mat1).values[i] + (*mat2).values[i];
    }

    return res;
}

matrix_sf *mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2)
{
    unsigned int r1, c1, r2, c2;

    unsigned int i, j, k;
    int tempSum;
    matrix_sf *prodMat;

    if (mat1 == NULL || mat2 == NULL)
    {
        return NULL;
    }

    r1 = (*mat1).num_rows;

    c1 = (*mat1).num_cols;
    r2 = (*mat2).num_rows;
    c2 = (*mat2).num_cols;

    if (c1 != r2)
    {
        return NULL;
    }

    prodMat = malloc(sizeof(matrix_sf) + r1 * c2 * sizeof(int));
    if (prodMat == NULL)
    {
        return NULL;
    }

    (*prodMat).name = '#';
    (*prodMat).num_rows = r1;

    (*prodMat).num_cols = c2;

    for (i = 0; i < r1; i++)
    {
        for (j = 0; j < c2; j++)
        {
            tempSum = 0;
            for (k = 0; k < c1; k++)
            {
                tempSum += (*mat1).values[i * c1 + k] * (*mat2).values[k * c2 + j];
            }
            (*prodMat).values[i * c2 + j] = tempSum;
        }
    }

    return prodMat;
}

matrix_sf *transpose_mat_sf(const matrix_sf *mat)
{
    unsigned int r, c;
    unsigned int i, j;

    matrix_sf *transMat;

    if (mat == NULL)
    {
        return NULL;
    }

    r = (*mat).num_rows;
    c = (*mat).num_cols;

    transMat = malloc(sizeof(matrix_sf) + r * c * sizeof(int));
    if (transMat == NULL)
    {
        return NULL;
    }

    (*transMat).name = '#';
    (*transMat).num_rows = c;

    (*transMat).num_cols = r;

    for (i = 0; i < r; i++)
    {
        for (j = 0; j < c; j++)
        {
            (*transMat).values[j * r + i] = (*mat).values[i * c + j];
        }
    }

    return transMat;
}

matrix_sf *create_matrix_sf(char name, const char *expr)
{
    unsigned int rows = 0;
    unsigned int cols = 0;

    unsigned int totalElements;
    unsigned int index = 0;
    matrix_sf *newMat;

    const char *p;
    int value;

    if (expr == NULL)
    {
        return NULL;
    }
    sscanf(expr, "%u %u", &rows, &cols);

    if (rows == 0 || cols == 0)
    {
        return NULL;
    }

    totalElements = rows * cols;

    newMat = malloc(sizeof(matrix_sf) + totalElements * sizeof(int));
    if (newMat == NULL)

    {
        return NULL;
    }

    (*newMat).name = name;
    (*newMat).num_rows = rows;
    (*newMat).num_cols = cols;
    p = expr;
    while (*p != '[' && *p != '\0')
    {
        p++;
    }

    if (*p == '[')
    {
        p++;
    }
    while (*p != '\0' && *p != ']' && index < totalElements)
    {
        if (sscanf(p, "%d", &value) == 1)
        {
            (*newMat).values[index] = value;
            index++;
        }

        while (*p != '\0' && *p != ' ' && *p != ';' && *p != ']')
        {
            p++;
        }

        while (*p == ' ' || *p == ';')
        {
            p++;
        }
    }

    return newMat;
}

char *infix2postfix_sf(char *infix)
{
    char stack[500];
    char output[500];

    int top = -1;
    int k = 0;
    int i = 0;

    char ch;

    if (infix == NULL)
    {
        return NULL;
    }

    while (infix[i] != '\0')
    {
        ch = infix[i];

        if (ch == ' ' || ch == '\n')
        {
            i++;
            continue;
        }

        if (ch >= 'A' && ch <= 'Z')
        {
            output[k] = ch;

            k++;
        }
        else if (ch == '(')
        {
            top++;
            stack[top] = ch;
        }
        else if (ch == ')')
        {
            while (top >= 0 && stack[top] != '(')
            {
                output[k] = stack[top];
                k++;

                top--;
            }
            if (top >= 0 && stack[top] == '(')
            {
                top--;
            }
        }
        else if (ch == '\'')
        {
            output[k] = ch;
            k++;
        }
        else if (ch == '*' || ch == '+')
        {
            while (top >= 0)
            {
                char prev = stack[top];

                int precPrev = 0;

                int precNew = 0;

                if (prev == '\'')
                    precPrev = 3;
                else if (prev == '*')
                    precPrev = 2;
                else if (prev == '+')
                    precPrev = 1;

                if (ch == '\'')
                    precNew = 3;
                else if (ch == '*')
                    precNew = 2;
                else if (ch == '+')
                    precNew = 1;

                if (precPrev >= precNew && prev != '(')
                {
                    output[k] = stack[top];
                    k++;
                    top--;
                }
                else
                {
                    break;
                }
            }

            top++;
            stack[top] = ch;
        }

        i++;
    }

    while (top >= 0)
    {
        output[k] = stack[top];
        k++;
        top--;
    }

    output[k] = '\0';

    char *result = malloc(strlen(output) + 1);
    strcpy(result, output);

    return result;
}

matrix_sf *evaluate_expr_sf(char name, char *expr, bst_sf *root)
{
    char *postfix;
    matrix_sf *stack[500];

    int top = -1;
    int i;

    char token;

    if (expr == NULL)
    {
        return NULL;
    }

    postfix = infix2postfix_sf(expr);
    if (postfix == NULL)
    {
        return NULL;
    }

    for (i = 0; postfix[i] != '\0'; i++)
    {
        token = postfix[i];

        if (token >= 'A' && token <= 'Z')
        {
            stack[++top] = find_bst_sf(token, root);
        }
        else if (token == '\'')
        {
            matrix_sf *temp = stack[top--];
            matrix_sf *res = transpose_mat_sf(temp);

            stack[++top] = res;
        }
        else if (token == '+' || token == '*')
        {
            matrix_sf *mat2 = stack[top--];

            matrix_sf *mat1 = stack[top--];
            matrix_sf *res;

            if (token == '+')
            {
                res = add_mats_sf(mat1, mat2);
            }
            else
            {
                res = mult_mats_sf(mat1, mat2);
            }

            stack[++top] = res;
        }
    }

    matrix_sf *finalMat = stack[top];

    unsigned int total = (*finalMat).num_rows * (*finalMat).num_cols;

    matrix_sf *resultCopy = malloc(sizeof(matrix_sf) + total * sizeof(int));
    if (resultCopy == NULL)
    {
        free(postfix);
        return NULL;
    }

    (*resultCopy).name = name;
    (*resultCopy).num_rows = (*finalMat).num_rows;

    (*resultCopy).num_cols = (*finalMat).num_cols;

    for (int i = 0; i < total; i++)
    {
        (*resultCopy).values[i] = (*finalMat).values[i];
    }

    free(postfix);
    return resultCopy;
}

matrix_sf *execute_script_sf(char *filename)
{
    FILE *fp;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    bst_sf *root = NULL;
    matrix_sf *lastMatrix = NULL;

    fp = fopen(filename, "r");
    if (fp == NULL)
    {
        return NULL;
    }

    while ((read = getline(&line, &len, fp)) != -1)
    {
        if (read < 3)
        {
            continue;
        }

        char matrixName = line[0];

        char *equalSign = strchr(line, '=');

        if (equalSign == NULL)
        {
            continue;
        }

        equalSign++;
        if (strchr(equalSign, '[') != NULL)
        {

            matrix_sf *m = create_matrix_sf(matrixName, equalSign);

            root = insert_bst_sf(m, root);

            lastMatrix = m;
        }
        else
        {

            matrix_sf *m = evaluate_expr_sf(matrixName, equalSign, root);

            root = insert_bst_sf(m, root);

            lastMatrix = m;
        }
    }

    if (line != NULL)
    {
        free(line);
    }

    fclose(fp);

    return lastMatrix;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[])
{
    matrix_sf *m = malloc(sizeof(matrix_sf) + num_rows * num_cols * sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows * num_cols * sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat)
{
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows * mat->num_cols; i++)
    {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows * mat->num_cols - 1)
            printf(" ");
    }
    printf("\n");
}