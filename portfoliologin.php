<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title>Portfolio Login</title>
</head>

<body>
    <?php
$callbackUrl = "http://www.bowealth.com";
$siteUrl = "https://wealthelite.in";
?>

<!-- <style type="text/css">
  #exampleModal.modal.fade {
    display: none;
}

#exampleModal.modal.fade.in {
    display: block;
    position: fixed;
    top: 0;
    right: 0;
    bottom: 0;
    left: 0;
    z-index: 1050;
    display: none;
    overflow: hidden;
    -webkit-overflow-scrolling: touch;
    outline: 0;
    background: #b0b0b0e6;
    text-align: center;
}

.modal.in .modal-dialog {
    -webkit-transform: translate(0,0);
    -ms-transform: translate(0,0);
    -o-transform: translate(0,0);
    transform: translate(0,0);
}

.modal-dialog {
    background: #e3e3e3;
    padding: 46px;
    width: 350px !important;
    margin: 0px auto;
    margin-top: 43px;
}

label.login-form-radio {
    float: left;
}

 .form-input-field {
    width: 80%;
    font-size: 13px;
    padding: 5px 8px;
    margin-bottom: 10px;
    float: left;
}

</style> -->




<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">

<header style="padding: 15px 0; background-color: #cccccc;">
    <div class="container">
       <div class="col-sm-12 text-center">
           <a href="index.php"> <img src="https://bowealth.com/wp-content/uploads/2022/01/Bridge-Ocean-Logo-New_300x120.png"> </a>
       </div>
    </div>
</header>


<section style="padding: 100px 0;">
<div class="container">
     <div class="row">

        <div class="col-sm-4">
            
            
        </div>
       <div class="col-sm-4" style="background-color: #dddddd">
          <div>
           <form name="signinForm" id="signinForm" action="<?php echo $siteUrl; ?>/Login/login/validate-user" method="POST" style="border: solid 0px #eee;padding: 10px; text-align: center;" onsubmit="return GetSubmitLogin();">



                <div style="width: fit-content;margin: auto; min-width: 300px; padding-top: 20px;">



                   



                    <label style="width: fit-content;margin-right: 2px; color: #000;">



                        <input type="radio" name="loginType" id="client" value="CLIENT" checked="checked" style="margin-right: 5px;margin-bottom: 15px;">Client



                    </label>



                    <label style="width: fit-content;margin-right: 2px; color: #000;">



                        <input type="radio" name="loginType" id="emp" value="EMPLOYEE" style="margin-right: 5px;margin-bottom: 15px;">Employee



                    </label>



                    <label style="width: fit-content;margin-right: 2px; color: #000;">



                        <input type="radio" name="loginType" id="advisor" value="ADVISOR" style="margin-right: 5px;margin-bottom: 15px;">Admin



                    </label>



                    <br>



                </div>



                <div style="width: fit-content; margin: auto;">



                    <label style="width: auto;margin-bottom: 0px; float: left; color: #000; padding-right: 10px;" for="inputEmail">Username</label>



                    <input style="width: 250px; font-size: 13px;padding: 5px 8px; margin-bottom: 5px; color: #000;" type="text" id="inputEmail" name="username" value="" placeholder="Enter your User Name" required>



                    <br>







                    <label style="width: auto;margin-bottom: 0px; float: left; color: #000; padding-right: 13px;" for="inputPassword">Password</label>



                    <input style="width: 250px; font-size: 13px;padding: 5px 8px; margin-bottom: 5px;" type="password" id="inputPassword" name="password" value="" placeholder="******" required>

                    <br>

                    <label style="width: 240px;margin-bottom: 15px;cursor: pointer; color: #000;" data-toggle="modal" data-target="#exampleModal" onclick="resetForgetPasswordForm()">Forgot Password?</label>

                    <button style="padding: 4px 8px;" type="submit">Login</button>

                    <input type="hidden" name="loginFor" id="loginFor" value="CLIENT">
                    <input type="hidden" name="callbackUrl" id="callbackUrl" value="<?php echo $callbackUrl; ?>">
                </div>



                <div style="text-align: center;font-size: 14px;width: 100%;font-weight: 700;">



                    <p style="color: #dc3545; display: none" id="loginPageAlertError"></p>



                    <p style="color: #28a745; display: none" id="loginPageAlertSuccess"></p>



                </div>



            </form>



            



            <div class="modal fade" id="exampleModal" tabindex="-1" role="dialog" aria-labelledby="exampleModalLabel" aria-hidden="true">



                <div class="modal-dialog" style="width: 430px;" role="document">



                    <div class="modal-content">



                        <div class="modal-header">



                            <h5 class="modal-title" style="float: left;width: 90%; color: #000000;">Forgot Password</h5>



                            <button type="button" class="close" data-dismiss="modal" aria-label="Close">



                                <span aria-hidden="true">&times;</span>



                            </button>



                        </div>



                        <div class="modal-body">



                            <div id="userNameDiv">

                            <form name="forgotForm" id="forgetPwdForm" method="POST" style="width: 380px;padding: 10px;margin: 10px auto;display: block;">



                                <div style="width: fit-content; margin: 0 auto;">



                                    <label style="width: 100px;line-height: 30px;margin: 0 auto; display: block; color: #fff;" for="inputEmail">Username</label>



                                    <div class="d-flex">



                                        <input style="width: 190px; font-size: 13px;padding: 5px 8px; color: #000;" type="text" id="inputEmail" name="userName" value="" placeholder="Enter your User Name" required>



                                        <input type="hidden" name="type" id="type" value="CLIENT">



                                        <input type="hidden" name="source" id="source" value="link" />



                                        <button style="padding: 4px 8px;margin-left: 10px;" type="button" onclick="submitForgetPwdUserNameForWeb();">Submit</button>



                                    </div>



                                </div>



                            </form>

                            </div>





                            <form name="forgotForm" id="forgetPwdFormVerify" method="POST" style="width: 380px;padding: 10px;margin: 10px auto;display: flex; display: none">



                                

                            </form>



                            <div style="text-align: center;font-size: 14px;width: 100%;font-weight: 700;">



                                <p style="color: #dc3545; display: none" id="loginPageAlertErrorFp"></p>



                                <p style="color: #28a745; display: none" id="loginPageAlertSuccessFp"></p>



                            </div>



                        </div>



                    </div>



                </div>



            </div>











            <?php



            $get = $_REQUEST;



            $type = "";



            $msg = "";



            if (isset($get['et'])) {



                $ET = $get['et'];



                $type = "";



                $msg = "";



                if ($ET == 1) {



                    $type = "error";



                    $msg = "Username or password is incorrect";



                }



                if ($ET == 2) {



                    $type = "error";



                    $msg = "Username not exists";



                }







                if ($ET == 3) {



                    $type = "error";



                    $msg = "Your IP was blocked! Please contact admin to access your account";



                }







                if ($ET == 4) {



                    $type = "error";



                    $msg = "Login disabled.";



                }



            }



            ?>
            </div>
       </div>
     </div>

        <div class="col-sm-4">
            
            
        </div>

</div>    
</section>

<footer style="padding: 15px 0; background-color: #cccccc;">
    
    <div class="container">
       <div class="col-sm-12 text-center">
            <p><b> BridgeOceans Wealth | Bridging Wealth Opportunities... </b> </p>
       </div>
    </div>

</footer>

<script src="https://code.jquery.com/jquery-3.1.1.min.js"></script>
<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js" ></script>
<script src="<?php echo $siteUrl; ?>/js/aes.js"></script>
<script src="<?php echo $siteUrl; ?>/js/login_script.js"></script>
<script src="<?php echo $siteUrl; ?>/js/formValidation.js"></script>
<script src="<?php echo $siteUrl; ?>/js/ajaxCore.js"></script>
<script src="<?php echo $siteUrl; ?>/js/login.js"></script>    

<script>
    var msgType = '<?php echo $type; ?>';
    var msg = '<?php echo $msg; ?>';
    if (msgType === "error" && msg !== "") {
        $('#loginPageAlertError').html(msg);
        $('#loginPageAlertError').css("display", "block");
    }

    $('input[type=radio][name=loginType]').change(function () {
        var optionValue = this.value;
        $('#loginFor').val('');
        if (optionValue != "" && optionValue != 'undefined') {
            $('#loginFor').val(optionValue);
            $('#type').val(optionValue);
        }
    });
</script>
<script>
$(document).ready(function(){
  $(".close").click(function(){
    $("#exampleModal").addClass("intro");
  });
});
</script>


</body>
</html>

