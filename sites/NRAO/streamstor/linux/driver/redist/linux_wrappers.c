/* Jungo Confidential. Copyright (c) 2011 Jungo Ltd.  http://www.jungo.com */

/* 
 * Source file for WinDriver Linux.
 *
 * This file may be distributed only as part of the 
 * application you are distributing, and only if it 
 * significantly contributes to the functionality of your 
 * application. (see \windriver\docs\license.txt for details).
 *
 * Web site: http://www.jungo.com
 * Email:    support@jungo.com
 */

#include "linux_common.h"
#if defined(LINUX_24_26)
    #include <linux/compiler.h>
#endif
#include <linux/pci.h>
#include <linux/init.h>
#include <linux/ioport.h>
#if defined(LINUX_24_26)
    #include <linux/completion.h>
    #include <linux/interrupt.h>
    #include <linux/mm.h>
#endif
#include <linux/sched.h>
#include <linux/pagemap.h>
#include <asm/io.h>
#include <asm/uaccess.h>
#include <asm/delay.h>
#if defined(LINUX_26)
    #include <asm/cacheflush.h>
    #include <linux/efi.h>
    #include <linux/smp_lock.h>
#endif
#if defined(LINUX_24)
    #include <linux/iobuf.h>
#endif
#if defined(LINUX_24_26)
    #include <linux/vmalloc.h>
#endif
#if defined(LINUX_20_22)
    #include <linux/malloc.h>
#endif
#include <asm/mman.h>
#include "linux_wrappers.h"
#if defined(LINUX_26) && (LINUX_VERSION_CODE < KERNEL_VERSION(2,6,13)) && \
    defined(CONFIG_COMPAT) && !defined(IA64)
        #include <linux/ioctl32.h>
#endif
#if defined(UDEV_SUPPORT)
    #include <linux/devfs_fs_kernel.h>
#endif

#include "wdusb_interface.h"

#if defined(WINDRIVER_KERNEL)
    #define REGISTER_FUNC_MOD static
#else
    /* When building the kernel plugin */
    #define REGISTER_FUNC_MOD extern
#endif
         
#if defined(WD_DRIVER_NAME_CHANGE)
REGISTER_FUNC_MOD kp_register_mod_func_t
    %DRIVER_NAME%_register_kp_module_func;
#else
REGISTER_FUNC_MOD kp_register_mod_func_t wd_register_kp_module_func;
#endif

#if defined(WINDRIVER_KERNEL) && (defined(LINUX_22_24_26))
    #define EXPORT_SYMTAB
#endif
#include <linux/module.h> // must come after #define EXPORT_SYMTAB

#if defined(MODVERSIONS)
    #include <linux/modversions.h>
#endif

#if defined(WINDRIVER_KERNEL) && defined(LINUX_20)
    #undef MODVERSIONS
    /* We don't support driver name change on Linux 2.0 */
    static struct symbol_table export_syms = {
        #include <linux/symtab_begin.h>
        X(wd_register_kp_module_func),
        #include <linux/symtab_end.h>
    };
#endif

#if defined(MODULE_LICENSE)
    MODULE_LICENSE("Proprietary");
#endif
#if defined(MODULE_AUTHOR)
    MODULE_AUTHOR("Jungo");
#endif
#if defined(MODULE_DESCRIPTION)
    #include "wd_ver.h"
    MODULE_DESCRIPTION("WinDriver v" WD_VERSION_STR " Jungo (C) 1999 - 2011");
#endif

#if defined(LINUX_22_24_26)
    void generic_pci_remove(void *dev_h, int notify);
    int generic_pci_probe(void *dev_h, int notify);
#else
    #include <linux/bios32.h>
#endif

#if defined(CONFIG_SWIOTLB) || defined(IA64)
    #define _CONFIG_SWIOTLB
#endif

#if defined(LINUX_24_26)
    #define VM_PG_OFFSET(vma) (vma)->vm_pgoff
    #define VM_BYTE_OFFSET(vma) ((vma)->vm_pgoff << PAGE_SHIFT)
#else
    #define VM_PG_OFFSET(vma) (vma)->vm_offset 
    #define VM_BYTE_OFFSET(vma) ((vma)->vm_offset << PAGE_SHIFT)
#endif

#define PAGE_COUNT(buf,size) ((((unsigned long)buf + size + PAGE_SIZE - 1) \
     >> PAGE_SHIFT) - ((unsigned long)buf >> PAGE_SHIFT))

#if defined(LINUX_24_26)
    static struct pci_dev *pci_root_dev;
#endif

typedef struct 
{
    struct page **pages;
    int page_count;
    unsigned long first_page_offset;
    unsigned long byte_count;
} LINUX_page_list;

typedef struct 
{
    unsigned char *map;
    unsigned long flags;
} LINUX_page_addr_param;

static inline int is_high_memory(unsigned long addr)
{
    return (addr >= (unsigned long)high_memory);
}

static inline int is_high_memory_phys(unsigned long phys_addr)
{
    return (phys_addr >= (unsigned long)virt_to_phys(high_memory));
}

int LINUX_down_interruptible(struct semaphore *sem)
{
    return down_interruptible(sem);
}

void LINUX_up(struct semaphore *sem)
{
    up(sem);
}

struct semaphore *LINUX_create_mutex(void)
{
    struct semaphore *sem = (struct semaphore *) kmalloc(sizeof(struct
        semaphore), GFP_ATOMIC);

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,37) || \
    (defined CONFIG_PREEMPT_RT && LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,31)) 
    sema_init(sem, 1);
#elif defined(LINUX_24_26)
    init_MUTEX(sem);
#else
    memset(sem,0,sizeof(struct semaphore));
#if defined(LINUX_22)
    atomic_set(&sem->count,1);
#else
    sem->count = 1;
#endif
#endif
    return sem;
}

void LINUX_free_mutex(struct semaphore *sem)
{
    kfree(sem);
}

void *LINUX_vmalloc(unsigned long size)
{
    return (void *) vmalloc(size);
}

void LINUX_vfree(void *addr)
{
    vfree(addr);
}

void *LINUX_kmalloc(unsigned int size, int flag)
{
    int is_atomic = flag & ATOMIC;
    int is_dma = flag & DMA;

    if (size > 128*1024)
        return NULL;
    flag = is_atomic ? GFP_ATOMIC : GFP_KERNEL;
    flag |= is_dma ? GFP_DMA : 0;
    return kmalloc((size_t) size, flag);
}

void *LINUX_dma_contig_alloc(void *pdev, unsigned int size, int flag,
    _u64 *phys)
{
#if defined(LINUX_26)
    struct pci_dev *hwdev = pdev;
    int gfp = ((flag & DMA) ? GFP_DMA : 0) | __GFP_NOWARN;
    #if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,6)
        gfp |= __GFP_COMP;
    #endif
    return dma_alloc_coherent(hwdev ? &hwdev->dev : NULL, size, 
        (dma_addr_t *)phys, GFP_KERNEL | gfp);
#else
    void *va;
    #if defined(LINUX_24)
        va = (void *)__get_free_pages((flag & DMA) ? GFP_DMA : 0,
            get_order(size));
    #else 
        va = LINUX_kmalloc(size, flag);
    #endif

    if (va && phys)
        *phys = LINUX_virt_to_phys(va);
    return va;
#endif
}

void LINUX_dma_contig_free(void *pdev, void *va, unsigned int size, _u64 phys)
{
#if defined(LINUX_26)
    struct pci_dev *hwdev = pdev; 

    dma_free_coherent(hwdev ? &hwdev->dev : NULL, size, va, phys);
#elif defined(LINUX_24)
    free_pages((unsigned long)va, get_order(size));
#else
    kfree(va);
#endif
}

void LINUX_kfree(const void *addr)
{
    kfree(addr);
}

unsigned long LINUX_copy_from_user(void *to, const void *from, unsigned long n)
{
    unsigned long rc = 0;
    #if defined(LINUX_26)
        rc = copy_from_user(to, from, n);
    #elif defined(LINUX_20)
        memcpy_fromfs(to, from, n);
    #elif defined(LINUX_22) || defined(LINUX_24)
        memcpy(to, from, n);
    #endif
    return rc;
}

unsigned long LINUX_copy_to_user(void *to, const void *from, unsigned long n)
{
    unsigned long rc = 0;
    #if defined(LINUX_26)
        rc = copy_to_user(to, from, n);
    #elif defined(LINUX_20)
        memcpy_tofs(to, from ,n);
    #elif defined(LINUX_22) || defined(LINUX_24)
        memcpy(to, from, n);
    #endif
    return rc;
}

void LINUX_MOD_INC_USE_COUNT(void)
{
#if !defined(LINUX_26)
    MOD_INC_USE_COUNT;
#endif
}

void LINUX_MOD_DEC_USE_COUNT(void)
{
#if !defined(LINUX_26)
    MOD_DEC_USE_COUNT;
#endif
}

#if defined(LINUX_26)
#if LINUX_VERSION_CODE <= KERNEL_VERSION(2,6,24)
struct page *windriver_vma_nopage(struct vm_area_struct *vma,
    unsigned long address, int *type)
#else 
int windriver_vma_fault(struct vm_area_struct *vma, struct vm_fault *vmf)
#endif
#elif defined(LINUX_24)
struct page *windriver_vma_nopage(struct vm_area_struct *vma,
    unsigned long address, int unused)
#else
unsigned long windriver_vma_nopage(struct vm_area_struct *vma,
    unsigned long address, int unused) 
#endif
{
#if LINUX_VERSION_CODE <= KERNEL_VERSION(2,6,24)
    unsigned long off = address - vma->vm_start;
#else 
    unsigned long off = (unsigned long)vmf->virtual_address - vma->vm_start;
#endif
    unsigned long va = (unsigned long)vma->vm_private_data + off;
    
#if defined(LINUX_24_26)
    struct page *page;
    
    page = virt_to_page(va);
    get_page(page); /* increment page count */

    #if defined(LINUX_26) && LINUX_VERSION_CODE <= KERNEL_VERSION(2,6,24)
    if (type)
        *type = VM_FAULT_MINOR;
    #endif
#if LINUX_VERSION_CODE <= KERNEL_VERSION(2,6,24)
    return page; 
#else
    vmf->page = page;
    return 0;
#endif
#else
    atomic_inc(&mem_map[MAP_NR(__pa(va))].count);
    return __pa(va);
#endif
}

struct vm_operations_struct windriver_vm_ops = {
#if LINUX_VERSION_CODE <= KERNEL_VERSION(2,6,24)
    nopage: windriver_vma_nopage,
#else
    fault: windriver_vma_fault,
#endif
};

#if LINUX_VERSION_CODE <= KERNEL_VERSION(2,6,9)
    #define REMAP_PAGE_RANGE remap_page_range
    #define REMAP_OFFSET(vma) VM_BYTE_OFFSET(vma)
#else
    #define REMAP_PAGE_RANGE remap_pfn_range
    #define REMAP_OFFSET(vma) VM_PG_OFFSET(vma)
#endif

#if defined(REMAP_API_CHANGE) || defined(LINUX_26)
    #define REMAP_ARG(vma) vma,
#else
    #define REMAP_ARG(vma) 
#endif

#ifndef pgprot_noncached /* define only for architectures supported by windriver */
#if defined(POWERPC) || defined(PPC64)
   #define pgprot_noncached(prot) (__pgprot(pgprot_val(prot) | _PAGE_NO_CACHE | _PAGE_GUARDED))
#elif defined(IA64)
   #define pgprot_noncached(prot) (__pgprot((pgprot_val(prot) & ~_PAGE_MA_MASK) | _PAGE_MA_UC))
#elif defined(__x86_64__)
   #define pgprot_noncached(prot) (__pgprot(pgprot_val(prot) | _PAGE_PCD | _PAGE_PWT))
#elif defined(__i386__)
   #define pgprot_noncached(prot) \
        (boot_cpu_data.x86 > 3 ? __pgprot(pgprot_val(prot) | _PAGE_PCD | _PAGE_PWT) : prot)
#endif 
#endif

#if defined(__i386__)
#ifndef cpu_has_mtrr
   #define cpu_has_mtrr (test_bit(X86_FEATURE_MTRR, boot_cpu_data.x86_capability))
#endif
#ifndef cpu_has_k6_mtrr
   #define cpu_has_k6_mtrr (test_bit(X86_FEATURE_K6_MTRR, boot_cpu_data.x86_capability))
#endif
#ifndef cpu_has_cyrix_arr
   #define cpu_has_cyrix_arr (test_bit(X86_FEATURE_CYRIX_ARR, boot_cpu_data.x86_capability))    
#endif
#ifndef cpu_has_centaur_mcr
   #define cpu_has_centaur_mcr (test_bit(X86_FEATURE_CENTAUR_MCR, boot_cpu_data.x86_capability))        
#endif                                                                                
#endif

#if defined(LINUX_24_26)
    static inline int uncached_access(struct file *file, unsigned long addr)
    {
    #if defined(__i386__)
        if (file->f_flags & O_SYNC)
                return 1;
        return !(cpu_has_mtrr || cpu_has_k6_mtrr || cpu_has_cyrix_arr ||
            cpu_has_centaur_mcr) && (addr >= __pa(high_memory));
    #elif defined(__x86_64__)
        if (file->f_flags & O_SYNC)
                return 1;
        return 0;
    #elif defined(CONFIG_IA64)
        #if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,11)
        return !(efi_mem_attributes(addr) & EFI_MEMORY_WB);
        #else
        return 1;
        #endif
    #elif defined(CONFIG_PPC64)
        return !page_is_ram(addr >> PAGE_SHIFT);
    #else
        if (file->f_flags & O_SYNC)
                return 1;
        return addr >= __pa(high_memory);
    #endif
    }
#else
    #define uncached_access(file, addr) 1
#endif

static int wd_mmap(
    #if defined(LINUX_20)
    struct inode *inode,
    #endif 
    struct file *file, struct vm_area_struct *vma)
{
#if LINUX_VERSION_CODE <= KERNEL_VERSION(2,4,14)
    vma->vm_flags |= VM_SHM | VM_LOCKED; /* don't swap out */
#else
    vma->vm_flags |= VM_RESERVED; /* don't swap out */
#endif

    if (file->private_data)
    {
        /* map DMA buffer */
        vma->vm_file = file;
        vma->vm_private_data = file->private_data; 
        vma->vm_ops = &windriver_vm_ops; /* use "nopage" method for system memory */
    }
    else
    {
        /* map IO buffer */
        vma->vm_flags |= VM_IO;
        if (uncached_access(file, VM_BYTE_OFFSET(vma)))
            vma->vm_page_prot = pgprot_noncached(vma->vm_page_prot);

        if (REMAP_PAGE_RANGE(
            REMAP_ARG(vma) vma->vm_start, REMAP_OFFSET(vma), vma->vm_end - vma->vm_start, vma->vm_page_prot))
        {
            return -EAGAIN;
        }
    #if defined(LINUX_20)
        vma->vm_inode = inode;
        inode->i_count++;
    #endif
    }
    return 0;
}

#if defined(WINDRIVER_KERNEL)
    #if defined(LINUX_20)
    void WDlinuxClose_20(struct inode *inode, struct file *filp)
    {
        WDlinuxClose(inode, filp);
    }
    #endif

    #if defined(CONFIG_COMPAT) && !defined(IA64)
        #if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,13)
            static int LINUX_ioctl_compat(unsigned int fd, unsigned int cmd,
                unsigned long arg, struct file *filep) 
        #else
            static long LINUX_ioctl_compat(struct file *filep, unsigned int cmd,
                unsigned long arg)
        #endif
            {
                struct inode *inode = filep->f_dentry->d_inode;
                return wd_linux_ioctl_compat(inode, filep, cmd, arg);
            }
    #endif

    #if defined(LINUX_24) && defined(CONFIG_COMPAT) && !defined(IA64)
    extern int register_ioctl32_conversion(unsigned int cmd,
        int (*handler)(unsigned int, unsigned int, unsigned long, struct file *));
    extern int unregister_ioctl32_conversion(unsigned int cmd);
    #endif

    int LINUX_register_ioctl32_conversion(unsigned int cmd)
    {
    #if defined(CONFIG_COMPAT) && !defined(IA64)
        #if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,13)
            return register_ioctl32_conversion(cmd, LINUX_ioctl_compat);
        #else
            return 0;
        #endif
    #else
        return -EINVAL;
    #endif
    }

    int LINUX_unregister_ioctl32_conversion(unsigned int cmd)
    {
    #if defined(CONFIG_COMPAT) && !defined(IA64)
        #if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,13)
            return unregister_ioctl32_conversion(cmd);
        #else
            return 0;
        #endif
    #else
        return -EINVAL;
    #endif
    }

    #if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,36)
        static long LINUX_unlocked_ioctl(struct file *filp, unsigned int cmd, 
            unsigned long args)
        {
            struct inode *inode = filp->f_dentry->d_inode;
            
            lock_kernel();
            return WDlinuxIoctl(inode, filp, cmd, args);
            unlock_kernel();
        }
    #endif

    struct file_operations windriver_fops = {
        #if defined(LINUX_24_26)
            owner: THIS_MODULE,
        #endif
        #if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,36)
        ioctl: WDlinuxIoctl,
        #else
        unlocked_ioctl: LINUX_unlocked_ioctl,
        #endif
        mmap: wd_mmap,
        open: WDlinuxOpen,
        #if defined(LINUX_22_24_26)
            release: WDlinuxClose,
        #else
            release: WDlinuxClose_20,
        #endif
        #if defined(CONFIG_COMPAT) && (LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,13))
            compat_ioctl: LINUX_ioctl_compat,
        #endif
    };

#if defined(UDEV_SUPPORT)
    static struct class_simple *windrvr_class = NULL;
#endif
    int LINUX_register_chrdev(unsigned int major, const char *name)
    {
#if defined(UDEV_SUPPORT)
        int err, new_major;

        new_major = register_chrdev(major, name, &windriver_fops);
        if (new_major < 0)
            return new_major;

        windrvr_class = class_simple_create(THIS_MODULE, (char*)name);
        if (IS_ERR(windrvr_class)) 
        {
            err = PTR_ERR(windrvr_class);
            printk("WinDriver: failed to create class (%d)\n", err);
            goto Error;
        }
        class_simple_device_add(windrvr_class, MKDEV(new_major, 0), NULL, name);
        err = devfs_mk_cdev(MKDEV(new_major, 0), S_IFCHR|S_IRUSR|S_IWUSR, 
            name);
        if (err)
        {
            printk("WinDriver: failed to make devfs node (%d)\n", err);
            goto Error;
        }
        return new_major;

Error:
        if (windrvr_class)
        {
            class_simple_device_remove(MKDEV(new_major,0));
            class_simple_destroy(windrvr_class);
        }
        unregister_chrdev(new_major, name);
        return err;
#else
        return register_chrdev(major, name, &windriver_fops);
#endif
    }
    
    void LINUX_unregister_chrdev(unsigned int major, const char *name)
    {
#if defined(UDEV_SUPPORT)
        devfs_remove(name);
        class_simple_device_remove(MKDEV(major, 0));
        class_simple_destroy(windrvr_class);
#endif
        unregister_chrdev(major, name);
    }
#endif

const char *LINUX_get_driver_name(void)
{
#if defined(WD_DRIVER_NAME_CHANGE)
    /* This section should only be compiled when building the
     * wizard generated kernel module */
    return "%DRIVER_NAME%";
#else
    return "windrvr6";
#endif
}

#if !defined(WINDRIVER_KERNEL)
    kp_register_mod_func_t LINUX_get_register_kp_module_func(void)
    {
    #if defined(WD_DRIVER_NAME_CHANGE)
        return %DRIVER_NAME%_register_kp_module_func;
    #else
        return wd_register_kp_module_func;
    #endif
    }
#else
    void unix_get_register_kp_module(kp_register_mod_func_t *func);

    void LINUX_init_register_kp_module_func(void)
    {
    #if defined(WD_DRIVER_NAME_CHANGE)
        unix_get_register_kp_module(&(%DRIVER_NAME%_register_kp_module_func));
    #else
        unix_get_register_kp_module(&(wd_register_kp_module_func));
    #endif
    }
#endif

#if !defined(WINDRIVER_KERNEL)
void LINUX_kp_inc_ref_count(void)
{
#if defined(LINUX_26)
    try_module_get(THIS_MODULE);
#else
    MOD_INC_USE_COUNT;
#endif
}

void LINUX_kp_dec_ref_count(void)
{
#if defined(LINUX_26)
    module_put(THIS_MODULE);
#else
    MOD_DEC_USE_COUNT;
#endif
}
#endif

#if defined(LINUX_26) && LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,20)
/* New work item API introduced kernel 2.6.20 */

struct work_handler_ctx
{
     struct work_struct bh;
     void (*func)(void *);
     void *func_data;
};

void work_handler_wrapper(struct work_struct *work)
{
    struct work_handler_ctx *handler_ctx = container_of(work, 
        struct work_handler_ctx, bh);

    if (!handler_ctx || !handler_ctx->func)
        return;

    handler_ctx->func(handler_ctx->func_data);
}

void *LINUX_bh_alloc(void (*routine)(void *), void *data)
{
    struct work_handler_ctx *ctx = (struct work_handler_ctx *) 
        vmalloc(sizeof(*ctx));

    if (!ctx)
        return NULL;

    memset(ctx, 0, sizeof(*ctx));
    ctx->func = routine;
    ctx->func_data = data;
    INIT_WORK(&ctx->bh, work_handler_wrapper);
    return (void *)(&ctx->bh);
}

void LINUX_bh_free(void *bh)
{
    struct work_struct *work = (struct work_struct *)bh;
    struct work_handler_ctx *ctx = container_of(work, struct work_handler_ctx, 
        bh);

    if (ctx)
        vfree(ctx);
}

#else

void *LINUX_bh_alloc(void (*routine)(void *), void *data)
{
#if defined(LINUX_26)
    struct work_struct *bh = (struct work_struct*) vmalloc(sizeof(*bh));
#else
    struct tq_struct *bh = (struct tq_struct *) vmalloc(sizeof(*bh));
#endif
    if (!bh)
        return NULL;
    memset(bh, 0, sizeof(*bh));
    bh->data = data;
#if defined(LINUX_26)
    bh->func = routine;
    bh->entry.next = &bh->entry;
    bh->entry.prev = &bh->entry;
#else
    bh->routine = routine;
#endif
    return (void *)bh;
}

void LINUX_bh_free(void *bh)
{
    vfree(bh);
}

#endif /* New work item API introduced kernel 2.6.20 */

void LINUX_flush_scheduled_tasks(void)
{
#if defined(LINUX_26)
    flush_scheduled_work();
#elif defined(LINUX_24)
    flush_scheduled_tasks();
#else
    // not used
#endif

}

void LINUX_schedule_task(void *bh)
{
#if defined(LINUX_26)
    schedule_work(bh);
#elif defined(LINUX_24)
    schedule_task(bh);
#else
    queue_task(bh, &tq_scheduler);
#endif
}

#if defined(WINDRIVER_KERNEL)

extern int wd_intr_handler_linux(void *context);

#if defined(LINUX_26)
irqreturn_t
#else
void
#endif
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,22)
wrapper_handler(int irq, void *ctx, struct pt_regs *pt)
#else
wrapper_handler(int irq, void *ctx)
#endif
{
#if defined(LINUX_26)
    int rc =
#endif
        wd_intr_handler_linux(ctx);
#if defined(LINUX_26)
    return rc;
#endif
}

int LINUX_request_irq(unsigned int irq, int is_shared, const char *device,
    void *ctx)
{
    unsigned long flag_disabled;
    unsigned long flag_shared;
    unsigned long flags;
        
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,22)
    flag_disabled = SA_INTERRUPT;
    flag_shared = SA_SHIRQ;
#else
    flag_disabled = IRQF_DISABLED;
    flag_shared = IRQF_SHARED;
#endif
    flags = flag_disabled;
    if (is_shared)
        flags |= flag_shared;
    return request_irq(irq, wrapper_handler, flags, device, ctx);
}

void LINUX_free_irq(unsigned int irq, void *ctx)
{
    free_irq(irq, ctx);
}

int LINUX_get_irq(void *pdev)
{
    return ((struct pci_dev *)pdev)->irq;
}

int LINUX_pci_enable_msi(void *pdev)
{
    #ifdef CONFIG_PCI_MSI
        return pci_enable_msi((struct pci_dev *)pdev);
    #else
        return -1;
    #endif
}

void LINUX_pci_disable_msi(void *pdev)
{
    #ifdef CONFIG_PCI_MSI
        pci_disable_msi((struct pci_dev *)pdev);
    #endif
}

#if defined(CONFIG_PCI_MSI)
    typedef struct msix_vectors_t
    {
        struct msix_vectors_t *next;
        struct msix_entry *entries;
        int num_entries;
        void *pdev;
    } msix_vectors_t;

    typedef struct
    {
        msix_vectors_t *list;
        spinlock_t lock;         
    } msix_vectors_list_t;

    static msix_vectors_list_t msix_vectors_list = {NULL, SPIN_LOCK_UNLOCKED};
#endif

int LINUX_request_irq_msix(void *pdev, int is_shared, const char *device,
    void *ctx)
{
#if defined(CONFIG_PCI_MSI)
    msix_vectors_t *v;
    int i, rc = 0;

    spin_lock_irq(&msix_vectors_list.lock);
    for (v = msix_vectors_list.list; v && v->pdev != pdev; v = v->next);
    spin_unlock_irq(&msix_vectors_list.lock);
    if (!v)
        return -1;
    for (i = 0; i < v->num_entries && !rc; i++)
        rc = LINUX_request_irq(v->entries[i].vector, is_shared, device, ctx);
    return rc;
#else
    return -1;
#endif
}

void LINUX_free_irq_msix(void *pdev, void *ctx)
{
#if defined(CONFIG_PCI_MSI)
    msix_vectors_t *v;
    int i;
        
    spin_lock_irq(&msix_vectors_list.lock);
    for (v = msix_vectors_list.list; v && v->pdev != pdev; v = v->next);
    spin_unlock_irq(&msix_vectors_list.lock);
    
    if (!v)
        return;
    for (i = 0; i < v->num_entries; i++)
        free_irq(v->entries[i].vector, ctx);
#endif
}

int LINUX_pci_enable_msix(void *pdev, int num_entries)
{
#if defined(CONFIG_PCI_MSI)
    int rc, i;
    struct msix_entry *entries;
    msix_vectors_t *v;

    entries = (struct msix_entry *)
        kmalloc(sizeof(struct msix_entry) * num_entries, GFP_KERNEL);
    if (!entries)
        return -ENOMEM;

    v = (msix_vectors_t *)kmalloc(sizeof(msix_vectors_t), GFP_KERNEL);
    if (!v)
    {
        kfree(entries);
        return -ENOMEM;
    }

    for (i = 0; i < num_entries; i++)
        entries[i].entry = i;

    rc = pci_enable_msix((struct pci_dev *)pdev, entries, num_entries);
    if (rc)
    {
        printk("%s: failed %d\n", __FUNCTION__, rc);
        kfree(entries);
        kfree(v);
        return rc;
    }

    v->entries = entries;
    v->num_entries = num_entries;
    v->pdev = pdev;
    spin_lock_irq(&msix_vectors_list.lock);
    v->next = msix_vectors_list.list;
    msix_vectors_list.list = v;
    spin_unlock_irq(&msix_vectors_list.lock);

    return 0;
#else
    return -1;
#endif
}

void LINUX_pci_disable_msix(void *pdev)
{
#if defined(CONFIG_PCI_MSI)
    msix_vectors_t **v, *tmp;

    spin_lock_irq(&msix_vectors_list.lock);
    for (v = &msix_vectors_list.list; *v && (*v)->pdev != pdev;
        v = &(*v)->next);
    if (!*v)
        return;
    tmp = *v;
    *v = (*v)->next;
    spin_unlock_irq(&msix_vectors_list.lock);

    kfree(tmp->entries);
        kfree(tmp);

    pci_disable_msix(pdev);
#endif
}

#endif /* WINDRIVER_KERNEL */

unsigned long LINUX_ioremap (unsigned long phys_addr, unsigned long size)
{
#if defined(LINUX_22_24_26)
    return (unsigned long)ioremap_nocache(phys_addr, size);
#else
    return (unsigned long)vremap(phys_addr, size);
#endif
}

void LINUX_iounmap(unsigned long addr)
{
#if !defined(IA64)
    if (!is_high_memory(addr))
        return;
#endif
    
#if defined(LINUX_22_24_26)
    iounmap((void *)addr);
#else
    vfree((void *)addr);
#endif
}

unsigned long LINUX_do_mmap(struct file *file, 
    unsigned long len, unsigned long phys_addr, void *kernel_addr)
{
    unsigned long addr;

#if LINUX_VERSION_CODE <= KERNEL_VERSION(2,4,2)
    down(&current->mm->mmap_sem);
#else
    down_write(&current->mm->mmap_sem);
#endif    
    /* when kernel address is not zero, this is a mapping of DMA buffer */
    file->private_data = kernel_addr;
    addr = do_mmap(file, 0, len, PROT_READ | PROT_WRITE, MAP_SHARED, phys_addr);
#if LINUX_VERSION_CODE <= KERNEL_VERSION(2,4,2)
    up(&current->mm->mmap_sem);
#else
    up_write(&current->mm->mmap_sem);
#endif
    return addr;
}

int LINUX_do_munmap(unsigned long addr, unsigned int len)
{
    return do_munmap(
#if defined(LINUX_24_26)           
        current->mm,
#endif
        addr, (size_t) len
#if defined(DO_MUNMAP_API_CHANGE)
        , 0
#endif
        );
}

int LINUX_pcibios_present(void)
{
#if defined(LINUX_20_22) 
    return pcibios_present();
#else
    return 1;   
#endif
}

#define GET_CUR_DEV(bus, dev_fn) \
    struct pci_dev *dev; \
    unsigned char old_bus=0; \
    unsigned char old_devfn=0; \
    int rc; \
    int fake = 0; \
    dev = LINUX_pci_find_slot(bus, dev_fn); \
    if (!dev) \
    { \
        fake = 1; \
        dev = pci_root_dev; \
        old_bus = dev->bus->number; \
        old_devfn = dev->devfn; \
        dev->bus->number = bus; \
        dev->devfn = dev_fn; \
    }

#define UNGET_CUR_DEV \
    if (fake) \
    { \
        dev->bus->number = old_bus; \
        dev->devfn = old_devfn; \
    } \

    
int LINUX_pcibios_read_config_byte (unsigned char bus, unsigned char dev_fn,
    unsigned int where, unsigned char *val)
{
#if defined(LINUX_24_26)
    GET_CUR_DEV(bus, dev_fn);
    rc = pci_read_config_byte(dev, where, val);
    UNGET_CUR_DEV;
    return rc;
#else
    return pcibios_read_config_byte(bus, dev_fn, (unsigned char)where, val); 
#endif
}

int LINUX_pcibios_read_config_word (unsigned char bus, unsigned char dev_fn,
    unsigned int where, unsigned short *val)
{
#if defined(LINUX_24_26)
    GET_CUR_DEV(bus, dev_fn);
    rc = pci_read_config_word(dev, where, val);
    UNGET_CUR_DEV;
    return rc;
#else
    return pcibios_read_config_word(bus, dev_fn, (unsigned char)where, val);
#endif
}

int LINUX_pcibios_read_config_dword (unsigned char bus, unsigned char dev_fn,
    unsigned int where, unsigned int *val)
{
#if defined(LINUX_24_26)
    GET_CUR_DEV(bus, dev_fn);
    rc = pci_read_config_dword(dev, where, val);
    UNGET_CUR_DEV;
    return rc;
#else
    return pcibios_read_config_dword(bus, dev_fn, (unsigned char)where, val);
#endif
}

int LINUX_pcibios_write_config_byte (unsigned char bus, unsigned char dev_fn,
    unsigned int where, unsigned char val)
{
#if defined(LINUX_24_26)
    GET_CUR_DEV(bus, dev_fn);
    rc = pci_write_config_byte(dev, where, val);
    UNGET_CUR_DEV;
    return rc;
#else
    return pcibios_write_config_byte(bus, dev_fn, (unsigned char)where, val);
#endif
}

int LINUX_pcibios_write_config_word (unsigned char bus, unsigned char dev_fn,
    unsigned int where, unsigned short val)
{
#if defined(LINUX_24_26)
    GET_CUR_DEV(bus, dev_fn);
    rc = pci_write_config_word(dev, where, val);
    UNGET_CUR_DEV;
    return rc;
#else
    return pcibios_write_config_word(bus, dev_fn, (unsigned char)where, val);
#endif
}

int LINUX_pcibios_write_config_dword (unsigned char bus, unsigned char dev_fn,
    unsigned int where, unsigned int val)
{
#if defined(LINUX_24_26)
    GET_CUR_DEV(bus, dev_fn);
    rc = pci_write_config_dword(dev, where, val);
    UNGET_CUR_DEV;
    return rc;
#else
    return pcibios_write_config_dword(bus, dev_fn, (unsigned char)where, val);
#endif
}

int LINUX_mmconfig_enabled(void)
{
#if defined(CONFIG_PCI_MMCONFIG) || defined(CONFIG_PCI_GOMMCONFIG) || \
    defined(CONFIG_PCI_GOANY)
    return 1;
#else
    LINUX_printk("LINUX_mmconfig_enabled: mmconfig is disabled\n");
    return 0;
#endif
}

void LINUX_udelay(unsigned long usecs)
{
    udelay(usecs);
}

void LINUX_schedule(void)
{
    schedule();
}

long LINUX_schedule_timeout(long timeout)
{
    set_current_state(TASK_INTERRUPTIBLE);
    return schedule_timeout(timeout);
}

#if defined(WINDRIVER_KERNEL)

    void LINUX_register_windriver_symboles()
    {
        #if defined(LINUX_20)
            register_symtab(&export_syms);
        #endif
    }
    #if defined(LINUX_26)
        #if defined(WD_DRIVER_NAME_CHANGE)
             EXPORT_SYMBOL(%DRIVER_NAME%_register_kp_module_func);
        #else
             EXPORT_SYMBOL(wd_register_kp_module_func);
        #endif
    #elif defined(LINUX_22) || defined(LINUX_24)
        #if defined(WD_DRIVER_NAME_CHANGE)
             EXPORT_SYMBOL_NOVERS(%DRIVER_NAME%_register_kp_module_func);
        #else 
             EXPORT_SYMBOL_NOVERS(wd_register_kp_module_func);
        #endif
    #endif
#endif

void LINUX_register_symboles()
{
    #if defined(LINUX_20)
        register_symtab(NULL);
    #elif defined(LINUX_24)
        EXPORT_NO_SYMBOLS;
    #endif
}

#if defined(LINUX_24_26) && defined(WINDRIVER_KERNEL)
#include <linux/init.h>

#if defined(WD_DRIVER_NAME_CHANGE)
    #define WD_PCI_DRIVER_NAME "%DRIVER_NAME%_pci"
#else
    #define WD_PCI_DRIVER_NAME "windrvr6_pci"
#endif

static int __devinit wrap_generic_pci_probe(struct pci_dev *dev, const struct pci_device_id *id)
{
    generic_pci_probe(dev, 0);
    return -ENODEV;
}

static void __devexit wrap_generic_pci_remove(struct pci_dev *dev)
{
    generic_pci_remove(dev, 0);
}

void LINUX_pci_get_pnp_data(void *dev_h, LINUX_pnp_data *p)
{
    struct pci_dev *dev = (struct pci_dev *)dev_h;
    unsigned char int_pin;        

    if ((0 == pci_read_config_byte(dev_h, PCI_INTERRUPT_PIN, &int_pin)) && int_pin)    
    p->irq = dev->irq;
    else
        p->irq = -1;
    p->devfn = dev->devfn;
    p->bus_num = dev->bus->number;
    p->dev_h = dev;
    p->vid = dev->vendor;
    p->did = dev->device;
    p->hdr_type = dev->hdr_type;
}

void LINUX_pci_set_irq(void *dev_h, unsigned char irq)
{
    struct pci_dev *dev = (struct pci_dev *)dev_h;
    dev->irq = irq;
}
    
static const struct pci_device_id all_pci_ids[] = { {

        /* we want monitoring all devices */
        vendor:         PCI_ANY_ID,
        device:         PCI_ANY_ID,
        subvendor:      PCI_ANY_ID,
        subdevice:      PCI_ANY_ID,

        }, { /* end: all zeroes */ }
};

MODULE_DEVICE_TABLE (pci, all_pci_ids);

static struct pci_driver generic_pci_driver = {
        name:           WD_PCI_DRIVER_NAME,
        id_table:       &all_pci_ids [0],

        probe:          wrap_generic_pci_probe,
        remove:         wrap_generic_pci_remove,

};

#endif

int init_module(void) 
{
#if defined(LINUX_24_26)
    int ret;
    
    ret = init_module_cpp();
    if (ret)
    {
        printk("init module failed, status %d\n", ret);
        return ret;
    }
     
    #if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,22)
    pci_root_dev = pci_find_device(PCI_ANY_ID, PCI_ANY_ID, NULL);
    #else
    pci_root_dev = pci_get_device(PCI_ANY_ID, PCI_ANY_ID, NULL);
    #endif
    if (!pci_root_dev)
    {
        printk("windrvr error: unable to obtain pci_root_dev\n");
        return -1;
    }

    #if defined(WINDRIVER_KERNEL)
        #if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,22)
            pci_module_init(&generic_pci_driver);
        #else
            ret = pci_register_driver(&generic_pci_driver);
            if (ret)
            {
                printk("pci_register_driver failed, status %d\n", ret);
                return ret;
            }
        #endif
    #endif
    return 0;
#endif
    return init_module_cpp();
}

void cleanup_module(void) 
{
#if defined(LINUX_26) && defined(WINDRIVER_KERNEL)
    pci_unregister_driver(&generic_pci_driver);
#endif
    cleanup_module_cpp();
}

unsigned int LINUX_get_version(void)
{
#if defined(LINUX_20)
    return 200;
#elif defined(LINUX_22)
    return 220;
#elif defined(LINUX_24)
    return 240;
#elif LINUX_VERSION_CODE < KERNEL_VERSION(2,6,6)
    return 260;
#else
    return 266;
#endif
}

unsigned int LINUX_need_copy_from_user(void)
{
#if defined(LINUX_20) || defined(LINUX_26)
    return 1;
#else
    return 0;
#endif
}

struct semaphore *LINUX_create_semaphore(void)
{
    struct semaphore *sem = (struct semaphore *) kmalloc(sizeof(struct
        semaphore), GFP_ATOMIC);
#if defined(LINUX_24_26)
    sema_init(sem, 0);
#else
    memset(sem,0,sizeof(struct semaphore));
#endif
    return sem;
}

void LINUX_free_semaphore(struct semaphore *sem)
{
    kfree((void *)sem);
}

unsigned long LINUX_jiffies()
{
    return jiffies;
}

long LINUX_get_time_in_sec()
{
    struct timeval tv;
    do_gettimeofday(&tv);
    return tv.tv_sec;
}

#if defined LINUX_20_22
int LINUX_printk(const char *fmt, ...)
{
    int p[12];
    int res;
    int i;
    va_list args;
    va_start(args, fmt);
    for (i=0; i<sizeof(p)/sizeof(*p); i++)
        p[i] = va_arg(args, int);
    res = printk(fmt, p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8], p[9], p[10], p[11]);
    va_end(args);
    return res;
}
#else
int LINUX_printk(const char *fmt, ...)
{
    static char p[1024];
    va_list ap;
    int n;
    va_start(ap, fmt);
    n = vsnprintf (p, 1024, fmt, ap);
    va_end(ap);
    printk(p);
    return n; 
}
#endif

int LINUX_sprintf(char *buf, const char *fmt, ...)
{
    int res;
    va_list args;
    va_start(args, fmt);
    res = vsprintf(buf, fmt, args);
    va_end(args);
    return res; 
}

int LINUX_snprintf(char *buf, unsigned long n, const char *fmt, ...)
{
    int res;
    va_list args;
    va_start(args, fmt);
#if defined(LINUX_24_26)
    res = vsnprintf(buf, n, fmt, args);
#else
    res = vsprintf(buf, fmt, args);
#endif
    va_end(args);
    return res;
}
    
int LINUX_vsprintf(char *buf, const char *fmt, va_list args)
{
    return vsprintf(buf, fmt, args);
}

int LINUX_vsnprintf(char *buf, unsigned long n, const char *fmt, va_list args)
{
#if defined(LINUX_24_26)
    return vsnprintf(buf, n, fmt, args);
#else
    return vsprintf(buf, fmt, args);
#endif
}

unsigned long LINUX_virt_to_phys(void *va)
{
    return virt_to_phys(va);
}

_u64 LINUX_pci_map_single(void *dev, void *va, unsigned long size,
    unsigned int dma_direction)
{
    dma_addr_t pa = pci_map_single(dev, va, size, (int)dma_direction);
#if defined(dma_mapping_error)
    if (pci_dma_mapping_error(pa))
#else
    if (!pa)
#endif
    {
        printk("%s: failed, va %p, pa %llx, size %lx\n", __FUNCTION__, va,
            (_u64)pa, size);
        pa = 0;
    }
    return (_u64)pa;
}

void LINUX_pci_unmap_single(void *dev, _u64 pa, unsigned long size,
    unsigned int dma_direction)
{
    pci_unmap_single(dev, (dma_addr_t)pa, size, (int)dma_direction);
}

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,4,0)
    #define pci_dma_sync_single_for_cpu(x...)
    #define pci_dma_sync_sg_for_cpu(x...)
    #define pci_dma_sync_single_for_device(x...)
    #define pci_dma_sync_sg_for_device(x...)
#elif LINUX_VERSION_CODE < KERNEL_VERSION(2,6,5)
    #define pci_dma_sync_single_for_cpu pci_dma_sync_single
    #define pci_dma_sync_sg_for_cpu pci_dma_sync_sg
    #define pci_dma_sync_single_for_device(x...)
    #define pci_dma_sync_sg_for_device(x...)
#endif

void LINUX_pci_dma_sync_single_for_cpu(void *dev, _u64 dma_addr,
    unsigned long size, unsigned int dma_direction)
{
    pci_dma_sync_single_for_cpu(dev, (dma_addr_t)dma_addr, size, (int)dma_direction);
}

void LINUX_pci_dma_sync_single_for_device(void *dev, _u64 dma_addr,
    unsigned long size, unsigned int dma_direction)
{
    pci_dma_sync_single_for_device(dev, (dma_addr_t)dma_addr, size,
        (int)dma_direction);
}

void LINUX_pci_dma_sync_sg_for_cpu(void *dev, void *dma_handle, int nelems,
    unsigned int dma_direction)
{
#if !defined(_CONFIG_SWIOTLB)
    pci_dma_sync_sg_for_cpu(dev, dma_handle, nelems, (int)dma_direction);
#else
    struct scatterlist *sgl = (struct scatterlist *)dma_handle;
    int i;
    for (i=0; i<nelems; i++)
        pci_dma_sync_single_for_cpu(dev, sg_dma_address(&sgl[i]), sg_dma_len(&sgl[i]), (int)dma_direction);
#endif
}

void LINUX_pci_dma_sync_sg_for_device(void *dev, void *dma_handle, int nelems,
    unsigned int dma_direction)
{
#if !defined(_CONFIG_SWIOTLB)
    pci_dma_sync_sg_for_device(dev, dma_handle, nelems, (int)dma_direction);
#else
    struct scatterlist *sgl = (struct scatterlist *)dma_handle;
    int i;
    for (i=0; i<nelems; i++)
        pci_dma_sync_single_for_device(dev, sg_dma_address(&sgl[i]), sg_dma_len(&sgl[i]), (int)dma_direction);
#endif
}

int LINUX_pci_set_dma_mask(void *dev, _u64 dma_mask)
{
#if defined(LINUX_24_26)
    int err = 0;

    err = pci_set_dma_mask(dev, dma_mask);
    if (err)
    {
        printk("%s: pci_set_dma_mask failed. err %d dma_mask %llx\n",
            __FUNCTION__, err, dma_mask);
        goto Exit;
    }

    #if defined(LINUX_26)
        err = pci_set_consistent_dma_mask(dev, dma_mask);
        if (err)
        {
            printk("%s: pci_set_consistent_dma_mask failed. err %d "
                "dma_mask %llx\n", __FUNCTION__, err, dma_mask);
            goto Exit;
        }
    #endif

Exit:
    return err;
#else
    return 0;
#endif
}

void LINUX_mem_map_reserve(void *addr, unsigned long size)
{
    struct page *page;

    if (!addr || !size)
        return;

    for (page = virt_to_page(addr); page <= virt_to_page(addr + size - 1); page++)
#if defined(LINUX_24_26)
        SetPageReserved(page);
#else
        set_bit(PG_reserved, &page->flags);
#endif
}
    
void LINUX_mem_map_unreserve(void *addr, unsigned long size)
{
    struct page *page;

    if (!addr || !size)
        return;

    for (page = virt_to_page(addr); page <= virt_to_page(addr + size - 1); page++)
#if defined(LINUX_24_26)
        ClearPageReserved(page);
#else
        clear_bit(PG_reserved, &page->flags);
#endif
}

unsigned long LINUX_usecs_to_jiffies(unsigned long usecs)
{
    struct timespec t;
    t.tv_sec = usecs / 1000000L;
    t.tv_nsec = (usecs - t.tv_sec * 1000000L) * 1000L;
    return timespec_to_jiffies(&t);
}

unsigned long LINUX_msecs_to_jiffies(unsigned long msecs)
{
    struct timespec t;
    t.tv_sec = msecs / 1000L;
    t.tv_nsec = (msecs - t.tv_sec * 1000L) * 1000000L;
    return timespec_to_jiffies(&t);
}

void LINUX_add_timer(struct timer_list *timer, unsigned long timeout_msecs)
{
    timer->expires = jiffies + LINUX_msecs_to_jiffies(timeout_msecs);
    add_timer(timer);
}

void LINUX_create_timer(struct timer_list **timer, 
    void (*timer_cb)(unsigned long), unsigned long ctx)
{
    *timer = vmalloc(sizeof(struct timer_list));
    init_timer(*timer);
    (*timer)->function = timer_cb;
    (*timer)->data = ctx;
}

void LINUX_del_timer(struct timer_list *timer)
{
    del_timer(timer);
}

void LINUX_destroy_timer(struct timer_list *timer)
{
    vfree(timer);
}

void LINUX_spin_lock_irqsave(os_spinlock_t *lock)
{
    spin_lock_irqsave((spinlock_t *)lock->spinlock, lock->flags);
}

void LINUX_spin_unlock_irqrestore(os_spinlock_t *lock)
{
    spin_unlock_irqrestore((spinlock_t *)lock->spinlock, lock->flags);
}

void LINUX_spin_lock_irq(os_spinlock_t *lock)
{
    spin_lock_irq((spinlock_t *)lock->spinlock);
}

void LINUX_spin_unlock_irq(os_spinlock_t *lock)
{
    spin_unlock_irq((spinlock_t *)lock->spinlock);
}

void LINUX_spin_lock_init(os_spinlock_t *lock)
{
    spinlock_t *sl;
    // adding 4 bytes since sizeof(spinlock_t) can be zero
    sl = kmalloc(sizeof(spinlock_t)+4, GFP_ATOMIC);
    spin_lock_init(sl);
    lock->spinlock = (void *)sl;
}

void LINUX_spin_lock_uninit(os_spinlock_t *lock)
{
    kfree(lock->spinlock);
    lock->spinlock = NULL;
}

int LINUX_user_page_list_get(void *buf, unsigned long bytes, void **pl_h)
{
    int rc = 0, res;
    LINUX_page_list *pl;
    unsigned int page_count = PAGE_COUNT(buf, bytes);
    struct page **pages = NULL;

    *pl_h = NULL;
    
    /* User attempted Overflow! */
    if ((buf + bytes) < buf)
    {
        printk("%s: user attempted overflow\n", __FUNCTION__);
        return -EINVAL;
    }

    if (!(pl = kmalloc(sizeof(LINUX_page_list), GFP_KERNEL)))
        return -ENOMEM;

    memset(pl, 0, sizeof(LINUX_page_list));
    if (!(pages = kmalloc(page_count * sizeof(*pages), GFP_KERNEL)))
    {
        rc = -ENOMEM;
        goto Error;
    }

    down_read(&current->mm->mmap_sem);
    
    res = get_user_pages(current, current->mm, (unsigned long)buf, page_count,
        1, /* read/write permission */
        1, /* force: only require the 'MAY' flag, e.g. allow "write" even to readonly page */
        pages,
        NULL);

    up_read(&current->mm->mmap_sem);

    pl->pages = pages;
    pl->page_count = page_count;
    pl->first_page_offset = (unsigned long)buf & (PAGE_SIZE - 1); 
    pl->byte_count = bytes;

    if (res != page_count)
    {
        rc = -EINVAL;
        goto Error;
    }

    *pl_h = pl;
    return 0;

Error:
    printk("%s: error %d\n", __FUNCTION__, rc);
    if (pl)
        LINUX_user_page_list_put(pl);
    return rc;
}

static void page_list_free(LINUX_page_list *pl)
{
    if (!pl)
        return;

    if (pl->pages)
        kfree(pl->pages);
    kfree(pl);
}

static void page_list_iterate(void *pl_h, void (*func)(struct page *))
{
    LINUX_page_list *pl = (LINUX_page_list *)pl_h;
    int i;

    if (!pl_h)
        return;

    for (i = 0; i < pl->page_count; i++)
        func(pl->pages[i]);
}

static void page_reserve_cb(struct page *page) { SetPageReserved(page); }
static void page_clear_cb(struct page *page) { ClearPageReserved(page); }
static void page_release_cb(struct page *page)
{
    if (!PageReserved(page))
        SetPageDirty(page);
    page_cache_release(page);
}

void LINUX_user_page_list_put(void *pl_h)
{
    page_list_iterate(pl_h, page_release_cb);
    page_list_free((LINUX_page_list *)pl_h);
}

void LINUX_page_list_lock(void *pl_h)
{
    page_list_iterate(pl_h, page_reserve_cb);
}

void LINUX_page_list_unlock(void *pl_h)
{
    page_list_iterate(pl_h, page_clear_cb);
}

#ifndef MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#endif

#define PL_OFFSET(initial_offset, offset) \
    (((initial_offset) + (offset)) & (~PAGE_MASK))
#define PL_INDEX(initial_offset, offset) \
    ((((initial_offset) + (offset)) & PAGE_MASK) >> PAGE_SHIFT)


static void *LINUX_page_addr_get(struct page *page, LINUX_page_addr_param *param)
{
    int is_page_high = PageHighMem(page);
    
    memset(param, 0, sizeof(LINUX_page_addr_param));
    
    if (is_page_high)
    {
#if LINUX_VERSION_CODE <= KERNEL_VERSION(2,4,20)
        return NULL;
#else
        if (!param)
            return NULL;

        local_irq_save(param->flags);        
        param->map = (void *)(kmap_atomic(page, KM_IRQ0));
        if (!param->map)
        {
            printk("%s: error, page_buf is NULL\n", __FUNCTION__);
            local_irq_restore(param->flags);
            return NULL;
        }

        return param->map;
#endif
    }
    
    return page_address(page);
}

void LINUX_page_addr_put(LINUX_page_addr_param *param)
{
    if (!param || !param->map)
        return;
#if LINUX_VERSION_CODE <= KERNEL_VERSION(2,4,20)
    return;
#else
    kunmap_atomic(param->map, KM_IRQ0);
    local_irq_restore(param->flags);
#endif
}

inline int LINUX_page_list_copy_inout(void *pl_h, unsigned long offset, 
    void *buf, unsigned long bytes, int is_in)
{
    LINUX_page_list *pl = (LINUX_page_list *)pl_h;
    unsigned long page_offset, page_index, bytes_to_copy = 0;
    struct page **pages;
    void *page_buf;

    if (!bytes || !pl)
        return -1;

    pages = pl->pages;
    bytes = MIN(bytes, pl->byte_count);
    page_offset = PL_OFFSET(pl->first_page_offset, offset);
    page_index = PL_INDEX(pl->first_page_offset, offset);

    while (bytes && page_index < pl->page_count)
    {
        LINUX_page_addr_param param;    

        /* The first copy is until the end of page */
        bytes_to_copy = MIN(bytes, PAGE_SIZE - page_offset);
        page_buf = LINUX_page_addr_get(pages[page_index], &param);
        if (!page_buf)
            return -1;
        
        page_buf+= page_offset;

        if (is_in)
            memcpy(page_buf, buf, bytes_to_copy); 
        else
            memcpy(buf, page_buf, bytes_to_copy); 
      
        LINUX_page_addr_put(&param);
        page_offset = 0;
        page_index++;
        buf += bytes_to_copy;
        bytes -= bytes_to_copy;
    }

    return 0;
}

/*
 * Copy from buffer to page list
 */
int LINUX_page_list_copyin(void *pl_h, unsigned long offset, 
    const void *src, unsigned long bytes)
{
    return LINUX_page_list_copy_inout(pl_h, offset, (void *)src, bytes, 1);
}

/*
 * Copy from page list to buffer
 */
int LINUX_page_list_copyout(void *pl_h, unsigned long offset,
    void *dst, unsigned long bytes)
{
    return LINUX_page_list_copy_inout(pl_h, offset, dst, bytes, 0);
}

static struct page *LINUX_sg_page(struct scatterlist *sgl)
{
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,24)
        return sg_page(sgl);
#else
        return sgl->page;
#endif
}

static void LINUX_sg_set_page(struct scatterlist *sgl, struct page *page,
        unsigned int len, unsigned int offset)
{
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,24)
    sg_set_page(sgl, page, len, offset);
#else
    sgl->offset = offset;
    sgl->page = page; 
    sgl->length = len;
#endif
}

static void LINUX_sg_init_table(struct scatterlist *sgl, unsigned int nents)
{
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,24)
    sg_init_table(sgl, nents);
#else
    memset(sgl, 0, sizeof(struct scatterlist) * nents);
#endif
}

#if defined(LINUX_26)
static int build_dma_list_26(LINUX_dma_page *page_list, void *buf, unsigned long size,
    unsigned int *dma_sglen, unsigned int dma_direction, void *dev_handle, void **dma_handle)
{
    int rc, i, offset;
    struct page **pages = NULL;
    struct scatterlist *sgl;
    unsigned int page_count = *dma_sglen;
    LINUX_page_list *pl = NULL;
#if defined(_CONFIG_SWIOTLB)
    dma_addr_t mask;
#endif

    *dma_sglen = 0;
    if (!(sgl = vmalloc(sizeof(struct scatterlist) * page_count)))
    {
        rc = -ENOMEM;
        goto Error;
    }

    rc = LINUX_user_page_list_get(buf, size, (void **)&pl);
    if (rc)
        goto Error;

    pages = pl->pages;
    page_count = pl->page_count;

    LINUX_sg_init_table(sgl, page_count);
    offset = ((unsigned long)buf) & (~PAGE_MASK);
    if (page_count > 1)
    {
        int length = PAGE_SIZE - offset;

        LINUX_sg_set_page(&sgl[0], pages[0], length, offset);

        size -= length;
        for (i = 1; i < page_count ; i++, size -= PAGE_SIZE) 
        {
            LINUX_sg_set_page(&sgl[i], pages[i],
                size < PAGE_SIZE ? size : PAGE_SIZE, 0);
        }
    }
    else
        LINUX_sg_set_page(&sgl[0], pages[0], size, offset);


    /* map sglist, dma_sglen <= page_count */
#if defined(_CONFIG_SWIOTLB)
    *dma_sglen = page_count;
    mask = dev_handle ? ((struct pci_dev *)dev_handle)->dma_mask : (dma_addr_t)-1;
#else
    *dma_sglen = pci_map_sg((struct pci_dev *)dev_handle, sgl, page_count,
        (int)dma_direction);
#endif
    if (!(*dma_sglen))
    {
        rc = -ENXIO;
        goto Error;
    }

    for (i=0; i<*dma_sglen; i++)
    {
#if defined(_CONFIG_SWIOTLB)
        void *va = page_address(LINUX_sg_page(&sgl[i])) + sgl[i].offset;
        dma_addr_t dma_addr = virt_to_phys(va);
        
        if (dma_addr & ~mask)
        {
            dma_addr = (dma_addr_t)LINUX_pci_map_single(dev_handle, va,
                sgl[i].length, dma_direction);
        }
            
        sg_dma_address(&sgl[i]) = dma_addr;
        sg_dma_len(&sgl[i]) = sgl[i].length;
#endif
        page_list[i].phys = sg_dma_address(&sgl[i]);
        page_list[i].size = sg_dma_len(&sgl[i]);
    }
    /* Release the page list without unlocking the user memory */
    page_list_free(pl);
    *dma_handle = sgl;
    return 0;

 Error:
    printk("%s: error %d\n", __FUNCTION__, rc);
    if (pl)
        LINUX_user_page_list_put(pl);
    if (sgl)
        vfree(sgl);

    return rc;
}
#endif

#if defined(LINUX_24)
static int build_dma_list_24(LINUX_dma_page *page_list, void *buf, 
    unsigned long size, unsigned int dma_direction, void *dev_handle, void **dma_handle)
{
    int rc, i;
    struct kiobuf *iobuf;
    unsigned int page_count = PAGE_COUNT(buf, size);
    unsigned long first_page_offset = (unsigned long)buf & (PAGE_SIZE - 1);

#if defined(KIOBUF_WITH_SIZE)
    int nbhs = KIO_MAX_SECTORS;
    rc = alloc_kiovec_sz(1, &iobuf ,&nbhs);
#else
    rc = alloc_kiovec(1, &iobuf);
#endif

    if (rc)
    {
        printk("failed allocate iobuf\n");
        goto Error;
    }

    rc = map_user_kiobuf(READ, iobuf, (unsigned long) buf, size);
    if (rc)
    {
        printk("failed lock user buffer %p, size %ld, dma_direction %x\n", 
            buf, size, dma_direction);
        goto Error;
    }

    for (i=0; i<iobuf->nr_pages; i++)
    {
        page_list[i].phys = (iobuf->maplist[i] - mem_map) * PAGE_SIZE;
        page_list[i].size = PAGE_SIZE; 
    }
    
    /* Fix the page offset and size acording to the input buffer */ 
    page_list[0].phys += first_page_offset;
    if (page_count == 1)
        page_list[0].size = size;
    else
    {
        page_list[0].size = PAGE_SIZE - first_page_offset;
        page_list[page_count - 1].size = size - PAGE_SIZE * (page_count - 2) - 
            page_list[0].size;
    }

    *dma_handle = iobuf;
    return 0;

Error:
#if defined(KIOBUF_WITH_SIZE)
    free_kiovec_sz(1, &iobuf, &nbhs);
#else
    free_kiovec(1, &iobuf);
#endif
    return rc;

}
#endif

int LINUX_build_sg_dma(LINUX_dma_page *page_list, unsigned int *dma_sglen, 
    void *buf, unsigned long size, unsigned int dma_direction, void *dev_handle, 
    void **dma_handle)
{
    unsigned int page_count = PAGE_COUNT(buf, size);
    int rc = 0;

#if defined(LINUX_26)
    rc = build_dma_list_26(page_list, buf, size, &page_count, (int)dma_direction, dev_handle,
        dma_handle);
#elif defined(LINUX_24)
    rc = build_dma_list_24(page_list, buf, size, (int)dma_direction, dev_handle, 
        dma_handle);
#else
    // Not suppored by older kernels
    rc = 0x2000000aL;
#endif 
    if (!rc)
        *dma_sglen = page_count;
    return rc;
}

int LINUX_free_sg_dma(void *dma_handle, void *buf, unsigned long size,
    unsigned int dma_direction, void *dev_handle)
{
#if defined(LINUX_26)
    int i;
    unsigned int page_count = PAGE_COUNT(buf, size);
    struct scatterlist *sgl = (struct scatterlist *)dma_handle;
    
#if !defined(_CONFIG_SWIOTLB)
    pci_unmap_sg((struct pci_dev *)dev_handle, sgl, page_count, (int)dma_direction);
#endif
    
    for (i=0; i < page_count; i++) 
    {
#if defined(_CONFIG_SWIOTLB)
        pci_unmap_single(dev_handle, sg_dma_address(&sgl[i]), sg_dma_len(&sgl[i]), (int)dma_direction);
#endif
        if (!PageReserved(LINUX_sg_page(&sgl[i])))
            SetPageDirty(LINUX_sg_page(&sgl[i]));
        page_cache_release(LINUX_sg_page(&sgl[i]));
    }
    vfree(sgl);
#elif defined(LINUX_24)
#if defined(KIOBUF_WITH_SIZE)
    int nbhs = KIO_MAX_SECTORS;
#endif
    struct kiobuf *iobuf = (struct kiobuf *)dma_handle;
    if (iobuf)
    {
        unmap_kiobuf(iobuf);
#if defined(KIOBUF_WITH_SIZE)
        free_kiovec_sz(1, &iobuf, &nbhs);
#else
        free_kiovec(1, &iobuf);
#endif
    }
#else
    // not supported by older kernels
    return 0x2000000aL;
#endif
    return 0;
}

int LINUX_atomic_inc(os_interlocked_t *val)
{
#if defined(POWERPC) || defined(PPC64)
    return atomic_inc_return((atomic_t *)val);
#else
    atomic_t *v = (atomic_t *)val;      
    atomic_inc(v);
    return v->counter;
#endif
}

int LINUX_atomic_dec(os_interlocked_t *val)
{
#if defined(POWERPC) || defined(PPC64)
    return atomic_dec_return((atomic_t *)val);
#else
    atomic_t *v = (atomic_t *)val;
    atomic_dec(v);
    return v->counter;
#endif
}

int LINUX_atomic_add(os_interlocked_t *val, int i)
{
#if defined(POWERPC) || defined(PPC64)
    return atomic_add_return(i, (atomic_t *)val); 
#else
    atomic_t *v = (atomic_t *)val;
    atomic_add(i, v);
    return v->counter;
#endif
}

int LINUX_atomic_read(os_interlocked_t *val)
{
    return atomic_read((atomic_t *)val);
}

void LINUX_atomic_set(os_interlocked_t *val, int i)
{
    atomic_set((atomic_t *)val, i);
}

int LINUX_atomic_xchg(os_interlocked_t *val, int i)
{
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,16)
    return atomic_xchg((atomic_t *)val, i);
#else
    int ret = atomic_read((atomic_t *)val);

    atomic_set((atomic_t *)val, i);
    return ret;
#endif
}

#if 0
int LINUX_atomic_cmpxchg(os_interlocked_t *val, int cmp, int i)
{
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,16)
    return atomic_cmpxchg((atomic_t *)val, cmp, i);
#endif
    return 0;
}
#endif

void LINUX_atomic_init(os_interlocked_t *val)
{
    atomic_set((atomic_t *)val, 0);
}

void LINUX_atomic_uninit(os_interlocked_t *val)
{
    val = val;
}

void LINUX_event_wait(struct semaphore **event)
{
    int rc;
    rc = down_interruptible(*event);
}

void LINUX_event_create(struct semaphore **event)
{
    *event = LINUX_create_mutex();
}

void LINUX_event_set(struct semaphore **event)
{
   up(*event);
}

void LINUX_event_destroy(struct semaphore **event)
{
    LINUX_free_mutex(*event);
}

void LINUX_pci_set_master(void *dev_h)
{
    pci_set_master(dev_h);
}

int LINUX_pci_enable_device(void *dev_h)
{
#if defined(LINUX_24_26)
    return pci_enable_device(dev_h);
#endif
    return 0;
}

void LINUX_pci_disable_device(void *dev_h)
{
#if defined(LINUX_24_26)
    pci_disable_device(dev_h);
#endif
}

int LINUX_pci_request_regions(void *dev_h, char *modulename)
{
#if defined(LINUX_24_26)
    return pci_request_regions(dev_h, modulename);
#endif
    return 0;
}

void LINUX_pci_release_regions(void *dev_h)
{
#if defined(LINUX_24_26)
    pci_release_regions(dev_h);
#endif
}

unsigned char LINUX_inb(unsigned short port)
{
    return inb(port);
}

unsigned short LINUX_inw(unsigned short port)
{
    return inw(port);
}

unsigned int LINUX_inl(unsigned short port)
{
    return inl(port);
}

void LINUX_outb(unsigned char value, unsigned short port)
{
    outb(value, port);
}

void LINUX_outw(unsigned short value, unsigned short port)
{
    outw(value, port);
}

void LINUX_outl(unsigned int value, unsigned short port)
{
    outl(value, port);
}

void LINUX_insb(unsigned short port, void *addr, unsigned long count)
{
    insb(port, addr, count);
}
void LINUX_insw(unsigned short port, void *addr, unsigned long count)
{
    insw(port, addr, count);
}

void LINUX_insl(unsigned short port, void *addr, unsigned long count)
{
    insl(port, addr, count);
}

void LINUX_outsb(unsigned short port, void *addr, unsigned long count)
{
   outsb(port, addr, count);
}

void LINUX_outsw(unsigned short port, void *addr, unsigned long count)
{
   outsw(port, addr, count);
}

void LINUX_outsl(unsigned short port, void *addr, unsigned long count)
{
   outsl(port, addr, count);
}

char *LINUX_strcpy(char *dest,const char *src)
{
    return strcpy(dest, src);
}

char *LINUX_strcat(char *dest,const char *src)
{
    return strcat(dest, src);
}

char *LINUX_strncat(char *dest,const char *src, unsigned long count)
{
    return strncat(dest, src, count);
}

int LINUX_strcmp(const char *cs,const char *ct)
{
    return strcmp(cs, ct);
}

int LINUX_strncmp(const char *cs,const char *ct, unsigned long count)
{
   return strncmp(cs, ct, count);
}

void *LINUX_memset(void *adrr ,int s, unsigned long count)
{
    return memset(adrr, s, count);
}

void *LINUX_memcpy(void *to, const void *from, unsigned long n)
{
   return memcpy(to, from, n);
}

int LINUX_memcmp(void *to, const void *from, unsigned long n)
{
   return memcmp(to, from, n);
}
unsigned long LINUX_strlen(const char *s)
{
    return strlen(s);
}
 
char *LINUX_strncpy(char *dest, const char *src, unsigned long count)
{
    return strncpy(dest, src, count);
}

#if defined(LINUX_24_26)
#define ITEM_MEMORY 2
#define ITEM_IO 3

int LINUX_pci_resource_type(void *dev_h, int i)
{
    struct pci_dev *dev = (struct pci_dev *)dev_h;
    int flags = pci_resource_flags (dev, i);
    if (flags & IORESOURCE_IO)
        return ITEM_IO;
    if (flags & IORESOURCE_MEM)
        return ITEM_MEMORY;
    return -1;
}

unsigned long LINUX_pci_resource_start(void *dev_h, int i)
{
    struct pci_dev *dev = (struct pci_dev *)dev_h;
    return pci_resource_start (dev, i);
}

unsigned long LINUX_pci_resource_len(void *dev_h, int i)
{
    struct pci_dev *dev = (struct pci_dev *)dev_h;
    return pci_resource_len (dev, i);
}
#else

int LINUX_pci_resource_type(void *dev_h, int i)
{
    return 0;
}

unsigned long LINUX_pci_resource_start(void *dev_h, int i)
{
    return 0;
}

unsigned long LINUX_pci_resource_len(void *dev_h, int i)
{
    return 0;
}
#endif

#if LINUX_VERSION_CODE > KERNEL_VERSION(2,6,22)
struct pci_dev *LINUX_pci_get_bus_and_slot(unsigned int bus, 
    unsigned int devfn)
{
    struct pci_dev *dev = NULL;
    
    while ((dev = pci_get_device(PCI_ANY_ID, PCI_ANY_ID, dev)) != NULL)
    {
        if (dev->bus->number == bus && dev->devfn == devfn)
            return dev;
    }
    return NULL;
}
#endif

void *LINUX_pci_find_slot(unsigned int bus, unsigned int devfn)
{
#if LINUX_VERSION_CODE <= KERNEL_VERSION(2,6,22)
    return pci_find_slot(bus, devfn);
#else
    #if defined(PPC) || defined(PPC64)
    /* pci_get_bus_and_slot() is limited to domain 0, for PPC use our version
     * to get all the pci devices. */
    return LINUX_pci_get_bus_and_slot(bus, devfn);
    #else
    return pci_get_bus_and_slot(bus, devfn);
    #endif
#endif
}

void *LINUX_pci_bus(void *dev_h)
{
    return ((struct pci_dev *)dev_h)->bus;
}

void *LINUX_pci_bus_subordinate(void *dev_h)
{
    return ((struct pci_dev *)dev_h)->subordinate;
}

#if defined(LINUX_26) && defined(CONFIG_HOTPLUG)
void *LINUX_pci_find_bus(int domain, int busnr)
{
    return pci_find_bus(domain, busnr);
}

int LINUX_pci_scan_slot(void *bus_h, int devfn)
{
    return pci_scan_slot((struct pci_bus *)bus_h, devfn); 
}

void LINUX_pci_bus_add_devices(void *bus_h)
{
    pci_bus_add_devices((struct pci_bus *)bus_h);
}

void LINUX_pci_bus_size_bridges(void *bus_h)
{
    pci_bus_size_bridges((struct pci_bus *)bus_h);
}

void LINUX_pci_bus_assign_resources(void *bus_h)
{
    pci_bus_assign_resources((struct pci_bus *)bus_h);
}

void LINUX_pci_remove_bus_device(void *dev_h)
{
    pci_remove_bus_device((struct pci_dev *)dev_h);
}

#else

void *LINUX_pci_find_bus(int domain, int busnr)
{
    return 0;
}

int LINUX_pci_scan_slot(void *bus_h, int devfn)
{
    return 0;
}

void LINUX_pci_bus_add_devices(void *bus_h)
{}

void LINUX_pci_bus_size_bridges(void *bus_h)
{}

void LINUX_pci_bus_assign_resources(void *bus_h)
{}

void LINUX_pci_remove_bus_device(void *dev_h)
{}
#endif

unsigned long LINUX_get_page_size(void) 
{
    return PAGE_SIZE;
}

unsigned long LINUX_get_page_shift(void) 
{
    return PAGE_SHIFT;
}

/*
 * The readX() and writeX() interface automatically swap bytes
 * on big endian hosts
 */
unsigned char LINUX_read8(volatile void *addr)
{
    return readb((void *)addr);
}

unsigned short LINUX_read16(volatile void *addr)
{
    return readw((void *)addr);
}

unsigned int LINUX_read32(volatile void *addr)
{
    return readl((void *)addr);
}

_u64 LINUX_read64(volatile void *addr)
{
#ifdef readq
    return readq((void *)addr);
#else /* readq is not defined for all platforms */
    return le64_to_cpu(*(volatile _u64 *)(addr));
#endif
}

void LINUX_write8(unsigned char val, volatile void *addr)
{
    writeb(val, addr);
}

void LINUX_write16(unsigned short val, volatile void *addr)
{
    writew(val, addr);
}

void LINUX_write32(unsigned int val, volatile void *addr)
{
    writel(val, addr);
}

void LINUX_write64(_u64 val, volatile void *addr)
{
#ifdef writeq
    writeq(val, addr);
#else /* writeq is not defined for all platforms */
    *(volatile _u64 *)(addr) = cpu_to_le64(val);
#endif
}

#if defined(WINDRIVER_KERNEL)

#if !defined(LINUX_USB_SUPPORT)

#define WD_NOT_IMPLEMENTED 0x2000000aL /* Taken from windrvr.h */

/* Dummy OS_XXX functions in order to be able to load WinDriver when USB is not
 * supported and WDUSB module is not loaded */
DWORD OS_register_devices(void **register_ctx, WDU_MATCH_TABLE *match_tables, 
    DWORD match_tabs_number)
{
    printk("OS_register_devices: Not supported on this platform\n");
    return WD_NOT_IMPLEMENTED; 
}

DWORD OS_unregister_devices(void *register_handle)
{
    printk("OS_unregister_devices: Not supported on this platform\n");
    return WD_NOT_IMPLEMENTED;
}
DWORD OS_get_device_info(HANDLE os_dev_h, void *buf, 
    DWORD *buf_size, DWORD active_config, DWORD active_interface, 
    DWORD active_setting, BOOL is_kernelmode, DWORD dwOptions)
{
    printk("OS_get_device_info: Not supported on this platform\n");
    return WD_NOT_IMPLEMENTED;
}

DWORD OS_set_interface(HANDLE os_dev_h, 
    WDU_ALTERNATE_SETTING **alt_setting_info, DWORD interface_num,
    DWORD alt_num)
{
    printk("OS_set_interface: Not supported on this platform\n");
    return WD_NOT_IMPLEMENTED;
}

DWORD OS_get_max_urb_transfer_size(BOOL high_speed, const pipe_t *pipe)
{
    printk("OS_get_max_urb_transfer_size: Not supported on this platform\n");
    return WD_NOT_IMPLEMENTED;
}

DWORD OS_open_pipe(HANDLE os_dev_h, 
    const WDU_ENDPOINT_DESCRIPTOR *endpoint_desc, pipe_t *pipe)
{
    printk("OS_open_pipe: Not supported on this platform\n");
    return WD_NOT_IMPLEMENTED;
}

DWORD OS_get_device_property(HANDLE os_dev_h, void *buf, DWORD *buf_size,
    WD_DEVICE_REGISTRY_PROPERTY prop)
{
    printk("OS_get_device_property: Not supported on this "
        "platform\n");
    return WD_NOT_IMPLEMENTED;
}

DWORD OS_close_device(HANDLE os_dev_h)
{
    printk("OS_close_device: Not supported on this platform\n");
    return WD_NOT_IMPLEMENTED;
}

DWORD OS_reset_pipe(HANDLE os_dev_h, pipe_t *pipe)
{
    printk("OS_reset_pipe: Not supported on this platform\n");
    return WD_NOT_IMPLEMENTED;
}

DWORD OS_selective_suspend(HANDLE os_dev_h, DWORD options)
{
    printk("OS_selective_suspend: Not supported on this platform\n");
    return WD_NOT_IMPLEMENTED;
}

DWORD OS_transfer(HANDLE os_dev_h, pipe_t *pipe, void *file_h, PRCHANDLE prc_h,
    DWORD is_read, DWORD options, void *buf, DWORD bytes,
    DWORD *bytes_transferred, UCHAR *setup_packet, DWORD tout,
    PVOID ioctl_context)
{
    printk("OS_transfer: Not supported on this platform\n");
    return WD_NOT_IMPLEMENTED;
}

DWORD OS_halt_transfer(void *os_trans_ctx)
{
    printk("OS_halt_transfer: Not supported on this platform\n");
    return WD_NOT_IMPLEMENTED;
}

BOOL OS_init()
{
    return TRUE;
}

void OS_uninit()
{
}

void OS_set_stream_context(HANDLE file_h, stream_context_t *context)
{
    printk("%s: Not supported on this platform\n", __FUNCTION__);
}

stream_context_t * OS_get_stream_context(HANDLE file_h)
{
    printk("%s: Not supported on this platform\n", __FUNCTION__);
    return (stream_context_t *)0;
}

DWORD OS_stream_request_insert(stream_t *stream, void *request)
{
    printk("%s: Not supported on this platform\n", __FUNCTION__);
    return WD_NOT_IMPLEMENTED;
}

BOOL OS_is_stream_requests_queue_empty(stream_t *stream)
{
    printk("%s: Not supported on this platform\n", __FUNCTION__);
    return TRUE;
}

DWORD OS_stream_transfer_create(HANDLE os_dev_h, pipe_t *pipe)
{
    printk("%s: Not supported on this platform\n", __FUNCTION__);
    return WD_NOT_IMPLEMENTED;
}

DWORD OS_stream_transfer_start(stream_t *stream)
{
    printk("%s: Not supported on this platform\n", __FUNCTION__);
    return WD_NOT_IMPLEMENTED;
}

void OS_stream_issue_new_transfers(void *ctx)
{   
    printk("%s: Not supported on this platform\n", __FUNCTION__);
}

DWORD OS_wakeup(HANDLE os_dev_h, DWORD options)
{
    printk("OS_wakeup: Not supported on this platform\n");
    return WD_NOT_IMPLEMENTED;
}

DWORD OS_reset_device(HANDLE os_dev_h, DWORD options)
{
    printk("OS_reset_device: Not supported on this platform\n");
    return WD_NOT_IMPLEMENTED;
}

int OS_num_pending_urbs(void *ctx)
{
    printk("OS_num_pending_urbs: Not supported on this platform\n");
    return WD_NOT_IMPLEMENTED;
}

void wdusb_register_callbacks(wdusb_callbacks_t *callbacks, int *wdusb_ver)
{
    *wdusb_ver = WD_VER;
}

#elif defined(WD_DRIVER_NAME_CHANGE)

void wdusb_register_callbacks(wdusb_callbacks_t *callbacks, int *wdusb_ver)
{
    WD_FUNC_NAME(wdusb_register_callbacks)(callbacks, wdusb_ver);
}

DWORD OS_register_devices(void **register_ctx, WDU_MATCH_TABLE *match_tables,
    DWORD match_tabs_number)
{
    return WD_FUNC_NAME(OS_register_devices)(register_ctx, match_tables,
        match_tabs_number);
}

DWORD OS_unregister_devices(void *register_handle)
{
    return WD_FUNC_NAME(OS_unregister_devices)(register_handle);
}

DWORD OS_get_device_property(HANDLE os_dev_h, void *buf, DWORD *buf_size, 
    WD_DEVICE_REGISTRY_PROPERTY prop)
{
    return WD_FUNC_NAME(OS_get_device_property)(os_dev_h, buf, buf_size, prop);
}

DWORD OS_get_device_info(HANDLE os_dev_h, void *buf, DWORD *buf_size,
    DWORD active_config, DWORD active_interface, DWORD active_setting,
    BOOL is_kernelmode, DWORD dwOptions)
{
    return WD_FUNC_NAME(OS_get_device_info)(os_dev_h, buf, buf_size, active_config,
        active_interface, active_setting, is_kernelmode, dwOptions);
}

DWORD OS_set_interface(HANDLE os_dev_h,
    WDU_ALTERNATE_SETTING **alt_setting_info, DWORD interface_num,
    DWORD alternate_setting)
{
    return WD_FUNC_NAME(OS_set_interface)(os_dev_h, alt_setting_info,
        interface_num, alternate_setting);
}

DWORD OS_get_max_urb_transfer_size(BOOL high_speed, const pipe_t *pipe)
{
    return WD_FUNC_NAME(OS_get_max_urb_transfer_size)(high_speed, pipe);
}

DWORD OS_open_pipe(HANDLE os_dev_h, 
    const WDU_ENDPOINT_DESCRIPTOR *endpoint_desc, pipe_t *pipe)
{
    return WD_FUNC_NAME(OS_open_pipe)(os_dev_h, endpoint_desc, pipe);
}

DWORD OS_close_device(HANDLE os_dev_h)
{
    return WD_FUNC_NAME(OS_close_device)(os_dev_h);
}

DWORD OS_reset_pipe(HANDLE os_dev_h, pipe_t *pipe)
{
    return WD_FUNC_NAME(OS_reset_pipe)(os_dev_h, pipe);
}

DWORD OS_transfer(HANDLE os_dev_h, pipe_t *pipe, HANDLE file_h,
    PRCHANDLE prc_h, DWORD is_read, DWORD options, PVOID buf, DWORD bytes,
    DWORD *bytes_transferred, UCHAR *setup_packet, DWORD timeout,
    PVOID ioctl_context)
{
    return WD_FUNC_NAME(OS_transfer)(os_dev_h, pipe, file_h, prc_h, is_read,
        options, buf, bytes, bytes_transferred, setup_packet, timeout,
        ioctl_context);
}

DWORD OS_stream_transfer_create(HANDLE os_dev_h, pipe_t *pipe)
{
    WD_FUNC_NAME(OS_stream_transfer_create)(os_dev_h, pipe);
}

DWORD OS_stream_transfer_start(stream_t *stream)
{
    return WD_FUNC_NAME(OS_stream_transfer_start)(stream);
}

DWORD OS_halt_transfer(void *os_trans_ctx)
{
    return WD_FUNC_NAME(OS_halt_transfer)(os_trans_ctx);
}

BOOL OS_init(void)
{
    return WD_FUNC_NAME(OS_init)();
}

void OS_uninit(void)
{
    WD_FUNC_NAME(OS_uninit)();
}

DWORD OS_wakeup(HANDLE os_dev_h, DWORD options)
{
    return WD_FUNC_NAME(OS_wakeup)(os_dev_h, options);
}

DWORD OS_reset_device(HANDLE os_dev_h, DWORD options)
{
    return WD_FUNC_NAME(OS_reset_device)(os_dev_h, options);
}

DWORD OS_selective_suspend(HANDLE os_dev_h, DWORD options)
{
    return WD_FUNC_NAME(OS_selective_suspend)(os_dev_h, options);
}

stream_context_t *OS_get_stream_context(HANDLE file_h)
{
    return WD_FUNC_NAME(OS_get_stream_context)(file_h);
}

void OS_set_stream_context(HANDLE file_h, stream_context_t *context)
{
    WD_FUNC_NAME(OS_set_stream_context)(file_h, context);
}

DWORD OS_stream_request_insert(stream_t *stream, void *request)
{
    return WD_FUNC_NAME(OS_stream_request_insert)(stream, request);
}

BOOL OS_is_stream_requests_queue_empty(stream_t *stream)
{
    return WD_FUNC_NAME(OS_is_stream_requests_queue_empty)(stream);
}

void OS_stream_issue_new_transfers(void *ctx)
{
    WD_FUNC_NAME(OS_stream_issue_new_transfers)(ctx);
}

int OS_num_pending_urbs(void *ctx)
{
    return WD_FUNC_NAME(OS_num_pending_urbs)(ctx);
}

#endif /* LINUX_USB_SUPPORT, WD_DRIVER_NAME_CHANGE */

#if defined(LINUX_USB_SUPPORT)
int KDBG_func_ap(unsigned long dwLevel, unsigned long dwSection,
    const char *format, va_list ap);
unsigned long Usb_device_attach(void *os_dev_h,
    unsigned long interface_num, unsigned long configuration_value);
unsigned long Usb_device_detach(void *os_dev_h);
trans_t *create_transfer(pipe_t *pipe, void *os_trans_ctx,
    void (*os_trans_ctx_destroy_cb)(void *));
unsigned long release_transfer(trans_t *trans);
unsigned long map_usb_error_status(int err);

int WD_FUNC_NAME(wd_kdbg_func)(unsigned long dwLevel, unsigned long dwSection,
    const char *format, ...)
{
    int rc;
    va_list ap;
    
    va_start(ap, format);
    rc = KDBG_func_ap(dwLevel, dwSection, format, ap);
    va_end(ap);
    return rc;
}
EXPORT_SYMBOL(WD_FUNC_NAME(wd_kdbg_func));

trans_t *WD_FUNC_NAME(wd_create_transfer)(pipe_t *pipe, void *os_trans_ctx,
    void (*os_trans_ctx_destroy_cb)(void *))
{
    return create_transfer(pipe, os_trans_ctx, os_trans_ctx_destroy_cb);
}
EXPORT_SYMBOL(WD_FUNC_NAME(wd_create_transfer));

unsigned long WD_FUNC_NAME(wd_release_transfer)(trans_t *trans)
{
    return release_transfer(trans);
}
EXPORT_SYMBOL(WD_FUNC_NAME(wd_release_transfer));

unsigned long WD_FUNC_NAME(wd_map_error_status)(int err)
{
    return map_usb_error_status(err);
}
EXPORT_SYMBOL(WD_FUNC_NAME(wd_map_error_status));

unsigned long WD_FUNC_NAME(wd_device_attach)(void *os_dev_h,
    unsigned long interface_num, unsigned long configuration_value)
{
    return Usb_device_attach(os_dev_h, interface_num, configuration_value);
}
EXPORT_SYMBOL(WD_FUNC_NAME(wd_device_attach));

unsigned long WD_FUNC_NAME(wd_device_detach)(void *os_dev_h)
{
    return Usb_device_detach(os_dev_h);
}
EXPORT_SYMBOL(WD_FUNC_NAME(wd_device_detach));

int WD_FUNC_NAME(wd_user_page_list_get)(void *buf, unsigned long bytes,
    void **pl_h)
{
    return LINUX_user_page_list_get(buf, bytes, pl_h);
}
EXPORT_SYMBOL(WD_FUNC_NAME(wd_user_page_list_get));

void WD_FUNC_NAME(wd_user_page_list_put)(void *pl_h)
{
    LINUX_user_page_list_put(pl_h);
}
EXPORT_SYMBOL(WD_FUNC_NAME(wd_user_page_list_put));

int WD_FUNC_NAME(wd_page_list_copyout)(void *pl_h, unsigned long offset,
    void *dst, unsigned long bytes)
{
    return LINUX_page_list_copyout(pl_h, offset, dst, bytes);
}
EXPORT_SYMBOL(WD_FUNC_NAME(wd_page_list_copyout));

int WD_FUNC_NAME(wd_page_list_copyin)(void *pl_h, unsigned long offset, 
    const void *src, unsigned long bytes)
{
    return LINUX_page_list_copyin(pl_h, offset, src, bytes);
}
EXPORT_SYMBOL(WD_FUNC_NAME(wd_page_list_copyin));

const char *WD_FUNC_NAME(wd_get_driver_name)(void)
{
    return LINUX_get_driver_name();
}
EXPORT_SYMBOL(WD_FUNC_NAME(wd_get_driver_name));
#endif /* LINUX_USB_SUPPORT */
#endif /* WINDRIVER_KERNEL */

