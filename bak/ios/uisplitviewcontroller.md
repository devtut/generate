---
metaTitle: "iOS - UISplitViewController"
description: "Interacting Between Master and Detail View using Delegates in Objective C"
---

# UISplitViewController



## Interacting Between Master and Detail View using Delegates in Objective C


`UISplitViewController` needs to the root view controller of your app’s window

**AppDelegate.m**

```swift
- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions 
{
self.window = [[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]]
self.window.backgroundColor = [UIColor blackColor];
[self.window makeKeyAndVisible];
self.window.clipsToBounds = YES;
SplitViewController *spView = [[SplitViewController alloc]init];
self.window.rootViewController = spView;
[self.window makeKeyAndVisible];
return YES;
}

```

Just create an object for your `UISplitVIewController` and set it as the rootViewController for your application.

**SplitViewController.h**

```swift
#import <UIKit/UIKit.h>
#import "MasterViewController.h"
#import "DetailViewController.h"
@interface ViewController : UISplitViewController
{
DetailViewController *detailVC;
MasterViewController *masterVC;
NSMutableArray *array;
}
@end

```

`MasterViewController` is a `UIViewController` that is set on the left side of the device you can set the width in `UISplitViewController` using `maximumPrimaryColumnWidth` and `DetailViewController` is on the Right side

**SplitViewController.m**

```swift
#import "ViewController.h"
#define ANIMATION_LENGTH 0.3
@interface ViewController ()
@end

@implementation ViewController
- (void)viewDidLoad 
{
[super viewDidLoad];
masterVC = [[MasterViewController alloc]init];
detailVC = [[DetailViewController alloc]init];
[masterVC setDetailDelegate:(id)detailVC];
NSArray *vcArray = [NSArray arrayWithObjects:masterVC, detailVC, nil];
self.preferredDisplayMode = UISplitViewControllerDisplayModeAutomatic;
self.viewControllers = vcArray;
self.delegate = (id)self;
self.presentsWithGesture = YES;
}

```

The master and detail `UIViewController`'s are added to an `NSArray` which is set to `self.viewControllers`. `self.preferredDisplayMode` is is the mode set for displaying of `MasterViewController` and `DetailViewController`  . `self.presentsWithGesture` enables swipe gesture for displaying `MasterViewController`

**MasterViewController.h**

```swift
#import <UIKit/UIKit.h>

@protocol DetailViewDelegate <NSObject>
@required
- (void)sendSelectedNavController:(UIViewController *)viewController;
@end

@interface MasterViewController : UIViewController
{
    UITableView *mainTableView;
    NSMutableArray *viewControllerArray;
}
@property (nonatomic, retain) id<DetailViewDelegate> detailDelegate; 
@end

```

Create a `DetailViewDelegate` Delegate with `sendSelectedNavController` method for sending the `UIViewControllers` to the `DetailViewController`. Then in `MasterViewController` an `UITableView`is created . The `ViewControllerArray` contains all the `UIViewControllers` that needs to be displayed in `DetailViewController`

**MasterViewController.m**

```swift
#import "MasterViewController.h"

@implementation MasterViewController
@synthesize detailDelegate;

-(void)viewDidLoad
{
[super viewDidLoad];

UIViewController *dashBoardVC = [[UIViewController alloc]init];
[dashBoardVC.view setBackgroundColor:[UIColor redColor]];
UIViewController *inventVC = [[UIViewController alloc]init];
[inventVC.view setBackgroundColor:[UIColor whiteColor]];
UIViewController *alarmVC = [[UIViewController alloc]init];
[alarmVC.view setBackgroundColor: [UIColor purpleColor]];
UIViewController *scanDeviceVC = [[UIViewController alloc]init];
[scanDeviceVC.view setBackgroundColor:[UIColor cyanColor]];
UIViewController *serverDetailVC = [[UIViewController alloc]init];
[serverDetailVC.view setBackgroundColor: [UIColor whiteColor]];
viewControllerArray = [[NSMutableArray alloc]initWithObjects:dashBoardVC,inventVC,alarmVC,scanDeviceVC,serverDetailVC,nil];
mainTableView = [[UITableView alloc]initWithFrame:CGRectMake(0, 50,self.view.frame.size.width, self.view.frame.size.height-50) style:UITableViewStylePlain];
[mainTableView setDelegate:(id)self];
[mainTableView setDataSource:(id)self];
[mainTableView setSeparatorStyle:UITableViewCellSeparatorStyleNone];
[mainTableView setScrollsToTop:NO];
[self.view addSubview:mainTableView];
}

- (CGFloat)tableView:(UITableView *)tableView
heightForRowAtIndexPath:(NSIndexPath *)indexPath
{
    return 100;
}
- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:    (NSInteger)section
{
    return [viewControllerArray count];
}

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return 1;    //count of section
}

- (UITableViewCell *)tableView:(UITableView *)tableView
     cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    NSString *cellId = [NSString stringWithFormat:@"Cell%li%ld",(long)indexPath.section,(long)indexPath.row];
UITableViewCell *cell =[tableView   dequeueReusableCellWithIdentifier:cellId];

if (cell == nil)
{
   cell = [[UITableViewCell alloc]initWithStyle:UITableViewCellStyleDefault reuseIdentifier:cellId];
}
[cell.contentView setBackgroundColor:[UIColor redColor]];
cell.textLabel.text =[NSString stringWithFormat:@"My VC at index %ld",(long)indexPath.row];
return cell;
}

- (void)tableView:(UITableView *)tableView
didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    [detailDelegate sendSelectedNavController:[viewControllerArray objectAtIndex:indexPath.row]];
}

@end

```

Created some `UIViewController` and added it to an `NSMutableArray`. The `UITableView` is initialized then on `didselectrowatindexpath` method I send a `UIViewController` to the `DetailViewController` using `detailDelegate` delegate with the corresponding `UIViewController` in the `NSMutableArray` as a parameter

**DetailViewController.h**

```swift
#import <UIKit/UIKit.h>

@interface DetailViewController : UIViewController<UICollectionViewDelegate>
{
    UIViewController *tempNav;
}
@end

```

**DetailViewController.m**

```swift
#import "DetailViewController.h"

@implementation DetailViewController
-(void)viewDidLoad
{
    [super viewDidLoad];
    [self.view setBackgroundColor:[UIColor whiteColor]];
}
-(void)sendSelectedNavController:(UIViewController *)navController
{
    NSArray *viewsToRemove = [self.view subviews];
    for (UIView *v in viewsToRemove) {
        [v removeFromSuperview];
    }
    tempNav = navController;
    [self.view addSubview:tempNav.view];
}
@end

```

The `sendSelectedNavController` is declared here with removing all the `UIView`'s in the `DetailViewController` and adding the passed `UIViewController` from the `MasterViewController`.



#### Remarks


In iOS 8 and later, you can use the `UISplitViewController` class on all iOS devices, in previous versions of iOS, the class is available only on iPad.
`UISplitViewController` is a container class like `UITabViewController`, `UINavigationController`. It separates the main view into two     `UIViewControllers` `masterViewController`(PrimaryViewController) and `detailViewController`(SecondaryViewController). we can send an `NSArray` with two `UIViewControllers` and Apple recommends `UISplitViewController` as a rootviewcontroller for your application. To interact between the `UIViewControllers` I use `NSNotificationCenter`.

